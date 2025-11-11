using Sprache;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;

namespace GLSLhelper
{
	public partial class GlslParser
	{
		private static readonly Parser<string> _numberWithTrailingDigit =
			from number in Parse.Number
			from trailingDot in Parse.Char('.')
			select number + trailingDot;

		private static readonly Parser<string> _parserNumber  = Parse.DecimalInvariant.Or(_numberWithTrailingDigit);
		private static readonly Parser<string> _parserComment = new CommentParser().AnyComment;

		private static readonly Parser<string> _parserString =
			from start in Parse.Char('"')
			from text in Parse.CharExcept("\"\r\n").Many().Text()
			from end in Parse.Char('"').Optional()
			select start + text + end;

		private static readonly Parser<string> _parserPreprocessor =
			from _ in Parse.Char('#')
			from rest in Parse.AnyChar.Except(Parse.LineEnd).Many().Text()
			select "#" + rest;

		private static readonly Parser<string> _parserIdentifier =
			Parse.Identifier(Parse.Char(GlslSpecification.IsIdentifierStartChar, "Identifier start"),
							 Parse.Char(GlslSpecification.IsIdentifierChar, "Identifier character"));

		private static readonly Parser<char> _parserOperator = Parse.Chars(GlslSpecification.Operators);
		private readonly Parser<IEnumerable<Token>> _tokenParser;

		public GlslParser()
		{
			var comment      = _parserComment.Select(value => new Token(TokenType.Comment, value));
			var quotedString = _parserString.Select(value => new Token(TokenType.QuotedString, value));
			var preprocessor = _parserPreprocessor.Select(value => new Token(TokenType.Preprocessor, value));
			var number       = _parserNumber.Select(value => new Token(TokenType.Number, value));
			var identifier   = _parserIdentifier.Select(value => new Token(GlslSpecification.GetReservedWordType(value), value));
			var op           = _parserOperator.Select(value => new Token(TokenType.Operator, value.ToString()));
			var token        = comment.Or(preprocessor).Or(quotedString).Or(number).Or(identifier).Or(op);

			_tokenParser     = token.Positioned().Token().XMany();
		}

		public IEnumerable<IToken> Tokenize(string text)
		{
			if (string.IsNullOrWhiteSpace(text))
			{
				yield break;
			}

			var allTokens       = new List<Token>();
			int currentPosition = 0;
			var lineRegex       = new Regex(@"(\r\n|\r|\n)"); // To split lines and keep delimiters

			var matches         = lineRegex.Matches(text);
			int lastMatchEnd    = 0;

			foreach (Match lineMatch in matches)
			{
				string line = text.Substring(lastMatchEnd, lineMatch.Index - lastMatchEnd);
				string lineEnding = lineMatch.Value;

				if (line.TrimStart().StartsWith("#"))
				{
					// Handle preprocessor line
					allTokens.AddRange(ParsePreprocessorLine(line, currentPosition));
				}
				else
				{
					// Handle normal code line
					var tokensOnLine = _tokenParser.TryParse(line);
					if (tokensOnLine.WasSuccessful)
					{
						foreach (var token in tokensOnLine.Value)
						{
							allTokens.Add(new Token(token.Type, token.Value, currentPosition + token.Start, token.Length));
						}
					}
				}
				currentPosition += line.Length + lineEnding.Length;
				lastMatchEnd = lineMatch.Index + lineMatch.Length;
			}
			// Handle the last line if no line ending or if the last line is not empty
			if (lastMatchEnd < text.Length || (matches.Count == 0 && text.Length > 0))
			{
				string lastLine = text.Substring(lastMatchEnd);
				if (lastLine.TrimStart().StartsWith("#"))
				{
					allTokens.AddRange(ParsePreprocessorLine(lastLine, currentPosition));
				}
				else
				{
					var tokensOnLine = _tokenParser.TryParse(lastLine);
					if (tokensOnLine.WasSuccessful)
					{
						foreach (var token in tokensOnLine.Value)
						{
							allTokens.Add(new Token(token.Type, token.Value, currentPosition + token.Start, token.Length));
						}
					}
				}
			}

			// --- Post-processing pass ---
			var userDefinedTypes = new HashSet<string>();
			var structRegex = new Regex(@"struct\s+([a-zA-Z_][a-zA-Z0-9_]*)\s*\{", RegexOptions.Compiled);
			foreach (Match match in structRegex.Matches(text))
			{
				userDefinedTypes.Add(match.Groups[1].Value);
			}

			var allTypeNames = new HashSet<string>(GlslSpecification.BuiltInTypes);
			allTypeNames.UnionWith(userDefinedTypes);

		var controlKeywords = new HashSet<string> { "if", "while", "for", "switch" };
		var storageQualifiers = new HashSet<string> { "uniform", "varying", "in", "out", "inout", "attribute", "const", "buffer", "shared" };
		var finalTokens     = new List<IToken>();

	for (int i = 0; i < allTokens.Count; ++i)
	{
		var currentToken = allTokens[i];
		var currentType  = currentToken.Type;
		var currentValue = currentToken.Value;

				if (currentType == TokenType.Identifier || currentType == TokenType.Function)
				{
					var nextToken = (i + 1 < allTokens.Count) ? allTokens[i + 1] : null;
					var prevToken = (i > 0) ? allTokens[i - 1] : null;

					// Skip post-processing for tokens in preprocessor lines (after # or Preprocessor directive)
					if (prevToken != null && (prevToken.Value == "#" || prevToken.Type == TokenType.Preprocessor))
					{
						finalTokens.Add(currentToken);
						continue;
					}

					// Rule 1: User-defined types (struct names)
					if (currentType == TokenType.Identifier && userDefinedTypes.Contains(currentValue))
					{
						finalTokens.Add(new Token(TokenType.UserDefinedType, currentValue, currentToken.Start, currentToken.Length));
						continue;
					}
					// Rule 2: Check if this identifier is a type name in a declaration context
					// e.g., "uniform texture2D var" - texture2D should stay as type (Identifier in this case represents an unknown type)
					if (prevToken != null && prevToken.Type == TokenType.Keyword && storageQualifiers.Contains(prevToken.Value))
					{
						// Current token is right after a storage qualifier, it should be a type
						// Keep it as-is (Keyword if it's a built-in type, Identifier if unknown)
						// Don't convert to Function or UserVariable here
						finalTokens.Add(currentToken);
						continue;
					}

					// Rule 3: Variable declarations
					// This handles cases like "sampler2D texture2D;" or "uniform sampler2D texture2D;" where the last texture2D is a variable
					if (prevToken != null)
					{
						bool isPrevTokenAType = (prevToken.Type == TokenType.Keyword && GlslSpecification.IsBuiltInType(prevToken.Value))
											 || (prevToken.Type == TokenType.Identifier) // Could be a type name like texture2D
											 || (prevToken.Type == TokenType.UserDefinedType && userDefinedTypes.Contains(prevToken.Value));

						if (isPrevTokenAType && nextToken?.Value != "(")
						{
							// This is a variable declaration
							finalTokens.Add(new Token(TokenType.UserVariable, currentValue, currentToken.Start, currentToken.Length));
							continue;
						}
					}

					// Rule 4: Function calls (including type constructors)
					// In GLSL, type names can be used as constructors: vec4(1.0), sampler2D(tex, samp)
					if (nextToken?.Value == "(" && !controlKeywords.Contains(currentValue))
					{
						finalTokens.Add(new Token(TokenType.Function, currentValue, currentToken.Start, currentToken.Length));
						continue;
					}
				}

				// Rule 5: Type constructors for Keyword types
				// Handle cases where built-in types are used as constructors
				if (currentType == TokenType.Keyword && allTypeNames.Contains(currentValue))
				{
					var nextToken = (i + 1 < allTokens.Count) ? allTokens[i + 1] : null;
					if (nextToken?.Value == "(")
					{
						finalTokens.Add(new Token(TokenType.Function, currentValue, currentToken.Start, currentToken.Length));
						continue;
					}
				}

				finalTokens.Add(currentToken);
			}
			foreach (var token in finalTokens)
			{
				yield return token;
			}
		}

		private IEnumerable<Token> ParsePreprocessorLine(string line, int lineStartPosition)
		{
			var trimmedLine = line.TrimStart();
			int hashIndex = line.IndexOf('#');
			if (hashIndex == -1) yield break; // Should not happen if StartsWith("#")

			// Token for '#'
			yield return new Token(TokenType.Preprocessor, "#", lineStartPosition + hashIndex, 1);

			// Parse the rest of the line using the main tokenParser
			// This will correctly parse "include", then "file.h" as a string, etc.
			var remainingLine         = line.Substring(hashIndex + 1);
			var tokensOnRemainingLine = _tokenParser.TryParse(remainingLine);
			if (tokensOnRemainingLine.WasSuccessful)
			{
				bool isFirstToken = true;
				foreach (var token in tokensOnRemainingLine.Value)
				{
					// The first identifier after '#' should be marked as Preprocessor (e.g., if, include, version)
					// All other tokens keep their original type (numbers, strings, etc.)
					if (isFirstToken && (token.Type == TokenType.Identifier || token.Type == TokenType.ControlKeyword || token.Type == TokenType.Keyword))
					{
						// Mark as Preprocessor since it's the first token after '#'
						yield return new Token(TokenType.Preprocessor, token.Value, lineStartPosition + hashIndex + 1 + token.Start, token.Length);
						isFirstToken = false;
						continue;
					}
					isFirstToken = false;
					yield return new Token(token.Type, token.Value, lineStartPosition + hashIndex + 1 + token.Start, token.Length);
				}
			}
		}
	}
}
