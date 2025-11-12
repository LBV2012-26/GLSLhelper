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
			
			// Collect struct names
			var structRegex      = new Regex(@"struct\s+([a-zA-Z_][a-zA-Z0-9_]*)\s*\{", RegexOptions.Compiled);
			foreach (Match match in structRegex.Matches(text))
			{
				userDefinedTypes.Add(match.Groups[1].Value);
			}
			
			// Collect uniform/buffer block names: layout(...) uniform BlockName { ... }
			var blockRegex = new Regex(@"(uniform|buffer)\s+([a-zA-Z_][a-zA-Z0-9_]*)\s*\{", RegexOptions.Compiled);
			foreach (Match match in blockRegex.Matches(text))
			{
				userDefinedTypes.Add(match.Groups[2].Value);
			}
			
			// Also collect in/out block names: in BlockName { ... }
			var ioBlockRegex = new Regex(@"(in|out)\s+([a-zA-Z_][a-zA-Z0-9_]*)\s*\{", RegexOptions.Compiled);
			foreach (Match match in ioBlockRegex.Matches(text))
			{
				userDefinedTypes.Add(match.Groups[2].Value);
			}

			// Step 2: Collect macros (#define MACRO_NAME)
			var macros = new HashSet<string>();
			var defineRegex = new Regex(@"#\s*define\s+([a-zA-Z_][a-zA-Z0-9_]*)", RegexOptions.Compiled);
			foreach (Match match in defineRegex.Matches(text))
			{
				macros.Add(match.Groups[1].Value);
			}

			// Step 2.5: Collect member variables (inside struct/buffer/uniform blocks)
			var memberVariables = new HashSet<string>();
			for (int i = 0; i < allTokens.Count; i++)
			{
				var token = allTokens[i];
				
				// Look for: struct Name { ... }
				// or: layout(...) uniform BlockName { ... }
				// or: layout(...) buffer BlockName { ... }
				bool isStructOrBlock = false;
				int searchStart = i;
				
				// Case 1: struct keyword
				if (token.Type == TokenType.Keyword && token.Value == "struct")
				{
					isStructOrBlock = true;
					searchStart = i + 1; // Skip "struct" keyword, start after it
				}
				// Case 2: uniform, buffer, in, out blocks
				// Pattern: (uniform|buffer|in|out) BlockName {
				// Must have an identifier (block name) directly followed by {
				else if (token.Type == TokenType.Keyword && (token.Value == "uniform" || token.Value == "buffer" || token.Value == "in" || token.Value == "out"))
				{
					// Look for pattern: keyword [identifier] {
					// The identifier must be followed immediately by {
					int checkJ = i + 1;
					
					// Skip to the first identifier
					while (checkJ < allTokens.Count && 
						   (allTokens[checkJ].Type == TokenType.Keyword || allTokens[checkJ].Value == "(" || allTokens[checkJ].Value == ")"))
					{
						++checkJ;
					}
					
					// Check if we have: identifier {
					if (checkJ < allTokens.Count && allTokens[checkJ].Type == TokenType.Identifier &&
						checkJ + 1 < allTokens.Count && allTokens[checkJ + 1].Value == "{")
					{
						isStructOrBlock = true;
						searchStart = i + 1; // Start after uniform/buffer/in/out keyword
					}
				}
				
				if (isStructOrBlock)
				{
					// Find the opening {
					int j = searchStart;
					while (j < allTokens.Count && allTokens[j].Value != "{")
						++j;
					
					if (j < allTokens.Count)
					{
						// Now we're inside the struct/buffer, collect members until closing }
						int braceDepth = 1;
						++j;
						
						while (j < allTokens.Count && braceDepth > 0)
						{
							if (allTokens[j].Value == "{")
								++braceDepth;
							else if (allTokens[j].Value == "}")
								--braceDepth;
							
							// Look for member variable pattern: type memberName;
							if (braceDepth == 1 && j > 0 && j + 1 < allTokens.Count)
							{
								var prevToken    = allTokens[j - 1];
								var currentToken = allTokens[j];
								var nextToken    = allTokens[j + 1];
								
								bool isPrevTokenAType = (prevToken.Type == TokenType.Keyword && GlslSpecification.IsBuiltInType(prevToken.Value))
													  || prevToken.Type == TokenType.CompoundType // vec3, mat4, sampler2D, etc.
													  || prevToken.Type == TokenType.Identifier;
								
								if (isPrevTokenAType && currentToken.Type == TokenType.Identifier &&
									nextToken.Value != "(" && !macros.Contains(currentToken.Value))
								{
									memberVariables.Add(currentToken.Value);
								}
							}
							
							++j;
						}
					}
				}
			}

			// Step 3: Collect function parameters and track which functions they belong to
			// functionParameters: parameterName -> set of functionNames it belongs to
			var functionParameters = new Dictionary<string, HashSet<string>>();
			for (int i = 0; i < allTokens.Count; ++i)
			{
				var token = allTokens[i];
				
				// Look for function definition pattern: type identifier (
				if (token.Value == "(" && i >= 2)
				{
					var prevToken     = allTokens[i - 1];
					var prevPrevToken = allTokens[i - 2];
					
					// Check if this looks like a function definition
					// Return type can be: Keyword (float, int), CompoundType (vec3, mat4), or Identifier (user type)
					bool isPrevTokenAType = (prevPrevToken.Type == TokenType.Keyword && GlslSpecification.IsBuiltInType(prevPrevToken.Value))
										  || prevPrevToken.Type == TokenType.CompoundType // vec3, mat4, sampler2D, etc.
										  || prevPrevToken.Type == TokenType.Identifier;
					
					if (isPrevTokenAType && prevToken.Type == TokenType.Identifier)
					{
						string functionName = prevToken.Value;
						
						// Check if this is followed by ) { (function definition, not declaration)
						int checkJ = i + 1;
						int checkParenDepth = 1;
						while (checkJ < allTokens.Count && checkParenDepth > 0)
						{
							if (allTokens[checkJ].Value == "(")
								++checkParenDepth;
							else if (allTokens[checkJ].Value == ")")
								--checkParenDepth;
							++checkJ;
						}
						
						// After closing ), look for {
						bool hasBody = false;
						while (checkJ < allTokens.Count && (allTokens[checkJ].Type == TokenType.Identifier || allTokens[checkJ].Type == TokenType.Keyword))
						{
							++checkJ; // Skip qualifiers like const
						}
						
						if (checkJ < allTokens.Count && allTokens[checkJ].Value == "{")
						{
							hasBody = true;
						}
						
						if (hasBody)
						{
							// This is a function definition, collect parameters inside ( )
							int parenDepth = 1;
							int j = i + 1;
							
							while (j < allTokens.Count && parenDepth > 0)
							{
								if (allTokens[j].Value == "(")
									++parenDepth;
								else if (allTokens[j].Value == ")")
									--parenDepth;
								
								// Look for parameter pattern: type paramName
								if (parenDepth == 1 && j > i + 1)
								{
									var paramPrevToken = allTokens[j - 1];
									var paramToken = allTokens[j];
									
									bool isParamType = (paramPrevToken.Type == TokenType.Keyword && GlslSpecification.IsBuiltInType(paramPrevToken.Value))
													 || paramPrevToken.Type == TokenType.CompoundType // vec3, mat4, sampler2D, etc.
													 || paramPrevToken.Type == TokenType.Identifier;
									
									if (isParamType && paramToken.Type == TokenType.Identifier &&
										paramToken.Value != "," && paramToken.Value != ")")
									{
										// Record this parameter belongs to this function (may belong to multiple functions)
										if (!functionParameters.ContainsKey(paramToken.Value))
										{
											functionParameters[paramToken.Value] = new HashSet<string>();
										}
										functionParameters[paramToken.Value].Add(functionName);
									}
								}
								
								++j;
							}
						}
					}
				}
			}

			// Step 4: Collect local variables and track their owning functions
			// localVariables: variableName -> set of functionNames it belongs to
			var localVariables = new Dictionary<string, HashSet<string>>();
			// functionRanges: (startIndex, endIndex, functionName)
			var functionRanges = new List<(int start, int end, string name)>();
			
			string currentFunctionName = null;
			int functionStartIndex = -1;
			int functionScopeDepth = 0;
			
			for (int i = 0; i < allTokens.Count; ++i)
			{
				var token = allTokens[i];
				
				// Detect function definition: type identifier ( ... ) {
				if (token.Value == "(" && i >= 2 && currentFunctionName == null)
				{
					var prevToken     = allTokens[i - 1];
					var prevPrevToken = allTokens[i - 2];
					
					bool isPrevTokenAType = (prevPrevToken.Type == TokenType.Keyword && GlslSpecification.IsBuiltInType(prevPrevToken.Value))
										  || prevPrevToken.Type == TokenType.CompoundType // vec3, mat4, etc.
										  || prevPrevToken.Type == TokenType.Identifier;
					
					if (isPrevTokenAType && prevToken.Type == TokenType.Identifier)
					{
						// Found a potential function definition
						// Look ahead to find the opening {
						int j = i + 1;
						int parenDepth = 1;
						while (j < allTokens.Count && parenDepth > 0)
						{
							if (allTokens[j].Value == "(")
								++parenDepth;
							else if (allTokens[j].Value == ")")
								--parenDepth;
							++j;
						}
						
						// After closing ), look for {
						while (j < allTokens.Count && allTokens[j].Type == TokenType.Identifier)
						{
							++j; // Skip const, etc.
						}
						
						if (j < allTokens.Count && allTokens[j].Value == "{")
						{
							// This is a function definition
							currentFunctionName = prevToken.Value;
							functionStartIndex  = i - 1;
							functionScopeDepth  = 0;
						}
					}
				}
				
				// Track scope depth within function
				if (currentFunctionName != null)
				{
					if (token.Value == "{")
					{
						++functionScopeDepth;
					}
					else if (token.Value == "}")
					{
						--functionScopeDepth;
						
						// Function ended
						if (functionScopeDepth == 0)
						{
							functionRanges.Add((functionStartIndex, i, currentFunctionName));
							currentFunctionName = null;
							functionStartIndex  = -1;
						}
					}
					
					// Collect local variables inside function (depth > 0)
					if (functionScopeDepth > 0 && i > 0)
					{
						var prevToken = allTokens[i - 1];
						var nextToken = (i + 1 < allTokens.Count) ? allTokens[i + 1] : null;
						
						bool isPrevTokenAType = (prevToken.Type == TokenType.Keyword && GlslSpecification.IsBuiltInType(prevToken.Value))
											 || (prevToken.Type == TokenType.CompoundType) // vec3, mat4, sampler2D, etc.
											 || (prevToken.Type == TokenType.Identifier && !macros.Contains(prevToken.Value));
						
						if (isPrevTokenAType && token.Type == TokenType.Identifier &&
							nextToken != null && nextToken.Value != "(" && !macros.Contains(token.Value))
						{
							// Record this variable belongs to current function (may belong to multiple functions)
							if (!localVariables.ContainsKey(token.Value))
							{
								localVariables[token.Value] = new HashSet<string>();
							}
							localVariables[token.Value].Add(currentFunctionName);
						}
					}
				}
			}

			// Step 5: Collect global variables (declared at scope depth 0)
			var globalVariables = new HashSet<string>();
			int scopeDepth      = 0;
			for (int i = 0; i < allTokens.Count; ++i)
			{
				var token = allTokens[i];

				// Track scope depth
				if (token.Value == "{")
				{
					++scopeDepth;
				}
				else if (token.Value == "}")
				{
					--scopeDepth;
					
					// Check if there's an identifier after } at global scope (block instance)
					// Pattern: } instanceName;
					if (scopeDepth == 0 && i + 1 < allTokens.Count)
					{
						var nextToken = allTokens[i + 1];
						if (nextToken.Type == TokenType.Identifier && !macros.Contains(nextToken.Value))
						{
							// Check if there's a ; after it
							if (i + 2 < allTokens.Count && allTokens[i + 2].Value == ";")
							{
								globalVariables.Add(nextToken.Value);
							}
						}
					}
				}

				// Only collect variables at global scope (depth 0)
				if (scopeDepth == 0 && i > 0)
				{
					var prevToken = allTokens[i - 1];
					var nextToken = (i + 1 < allTokens.Count) ? allTokens[i + 1] : null;

					// Check if this is a variable declaration: type variableName
					bool isPrevTokenAType = (prevToken.Type == TokenType.Keyword && GlslSpecification.IsBuiltInType(prevToken.Value))
										 || (prevToken.Type == TokenType.CompoundType) // vec3, mat4, sampler2D, etc.
										 || (prevToken.Type == TokenType.Identifier && !macros.Contains(prevToken.Value) && userDefinedTypes.Contains(prevToken.Value));

					if (isPrevTokenAType && token.Type == TokenType.Identifier &&
						nextToken != null && nextToken.Value != "(" && !macros.Contains(token.Value))
					{
						globalVariables.Add(token.Value);
					}
				}
			}

			// Helper function to find which function a token belongs to
			string GetFunctionAtIndex(int tokenIndex)
			{
				foreach (var (start, end, name) in functionRanges)
				{
					if (tokenIndex >= start && tokenIndex <= end)
						return name;
				}
				return null;
			}

			var allTypeNames = new HashSet<string>(GlslSpecification.BuiltInTypes);
			allTypeNames.UnionWith(userDefinedTypes);

			var controlKeywords   = new HashSet<string> { "if", "while", "for", "switch" };
			var storageQualifiers = new HashSet<string> { "uniform", "varying", "in", "out", "inout", "attribute", "const", "buffer", "shared" };
			var finalTokens       = new List<IToken>();
			for (int i = 0; i < allTokens.Count; ++i)
			{
				var currentToken = allTokens[i];
				var currentType  = currentToken.Type;
				var currentValue = currentToken.Value;

				if (currentType == TokenType.Identifier || currentType == TokenType.Function)
				{
					var nextToken = (i + 1 < allTokens.Count) ? allTokens[i + 1] : null;
					var prevToken = (i > 0) ? allTokens[i - 1] : null;

					// Special handling for tokens in preprocessor lines
					if (prevToken != null && (prevToken.Value == "#" || prevToken.Type == TokenType.Preprocessor))
					{
						// Check if this is a macro definition: #define MACRO_NAME
						if (prevToken.Type == TokenType.Preprocessor && prevToken.Value == "define" &&
							currentType == TokenType.Identifier && macros.Contains(currentValue))
						{
							// This is the macro name in #define, color it as Macro
							finalTokens.Add(new Token(TokenType.Macro, currentValue, currentToken.Start, currentToken.Length));
							continue;
						}

						// For other preprocessor tokens, skip post-processing
						finalTokens.Add(currentToken);
						continue;
					}

					// Rule 0: Macros (highest priority for identifiers in normal code)
					if (currentType == TokenType.Identifier && macros.Contains(currentValue))
					{
						finalTokens.Add(new Token(TokenType.Macro, currentValue, currentToken.Start, currentToken.Length));
						continue;
					}

					// Rule 0.1: User-defined types (struct names) - High priority to preserve struct coloring
					if (currentType == TokenType.Identifier && userDefinedTypes.Contains(currentValue))
					{
						finalTokens.Add(new Token(TokenType.UserDefinedType, currentValue, currentToken.Start, currentToken.Length));
						continue;
					}

					// Rule 0.4: Member variables (check before function parameters)
					if (currentType == TokenType.Identifier && memberVariables.Contains(currentValue))
					{
						finalTokens.Add(new Token(TokenType.MemberVariable, currentValue, currentToken.Start, currentToken.Length));
						continue;
					}

					// Rule 0.5: Function parameters (check before other variable rules)
					// Only apply if the parameter belongs to the current function
					if (currentType == TokenType.Identifier && functionParameters.TryGetValue(currentValue, out HashSet<string> paramBelongsToFunctions))
					{
						string currentFunc = GetFunctionAtIndex(i);
						if (currentFunc != null && paramBelongsToFunctions.Contains(currentFunc))
						{
							finalTokens.Add(new Token(TokenType.FunctionParameter, currentValue, currentToken.Start, currentToken.Length));
							continue;
						}
					}

					// Rule 0.6: Local variables (check if variable belongs to current function)
					if (currentType == TokenType.Identifier && localVariables.TryGetValue(currentValue, out HashSet<string> belongsToFunctions))
					{
						string currentFunc = GetFunctionAtIndex(i);
						if (currentFunc != null && belongsToFunctions.Contains(currentFunc))
						{
							// This variable is used within its owning function
							finalTokens.Add(new Token(TokenType.UserVariable, currentValue, currentToken.Start, currentToken.Length));
							continue;
						}
					}

					// Rule 0.7: Global variables (check before other variable rules)
					if (currentType == TokenType.Identifier && globalVariables.Contains(currentValue))
					{
						finalTokens.Add(new Token(TokenType.GlobalVariable, currentValue, currentToken.Start, currentToken.Length));
						continue;
					}

					// Rule 2: Check if this identifier is a type name in a declaration context
					// e.g., "uniform texture2D var" - texture2D should stay as type (Identifier in this case represents an unknown type)
					if (prevToken != null && prevToken.Type == TokenType.Keyword && storageQualifiers.Contains(prevToken.Value))
					{
						// Current token is right after a storage qualifier, it should be a type
						// If it's a built-in type but wasn't recognized as Keyword, convert it
						if (currentType == TokenType.Identifier && GlslSpecification.IsBuiltInType(currentValue))
						{
							finalTokens.Add(new Token(TokenType.Keyword, currentValue, currentToken.Start, currentToken.Length));
						}
						else
						{
							// Keep it as-is (Keyword if it's a built-in type, Identifier if unknown)
							finalTokens.Add(currentToken);
						}
						continue;
					}

					// Rule 3: Variable declarations
					// This handles cases like "sampler2D texture2D;" or "uniform sampler2D texture2D;" where the last texture2D is a variable
					if (prevToken != null)
					{
						bool isPrevTokenAType = (prevToken.Type == TokenType.Keyword && GlslSpecification.IsBuiltInType(prevToken.Value))
											 || (prevToken.Type == TokenType.CompoundType) // vec3, mat4, sampler2D, etc.
											 || (prevToken.Type == TokenType.Identifier) // Could be a type name
											 || (prevToken.Type == TokenType.UserDefinedType && userDefinedTypes.Contains(prevToken.Value));

						if (isPrevTokenAType && nextToken?.Value != "(")
						{
							// Check if this is a member variable declaration
							if (memberVariables.Contains(currentValue))
							{
								finalTokens.Add(new Token(TokenType.MemberVariable, currentValue, currentToken.Start, currentToken.Length));
							}
							else
							{
								string currentFunc = GetFunctionAtIndex(i);
								
								// Check if this is a function parameter declaration
								if (functionParameters.TryGetValue(currentValue, out HashSet<string> paramFuncs) && 
									currentFunc != null && paramFuncs.Contains(currentFunc))
								{
									finalTokens.Add(new Token(TokenType.FunctionParameter, currentValue, currentToken.Start, currentToken.Length));
								}
								// Check if this is a global variable declaration
								else if (globalVariables.Contains(currentValue))
								{
									finalTokens.Add(new Token(TokenType.GlobalVariable, currentValue, currentToken.Start, currentToken.Length));
								}
								// Check if this is a local variable declaration within its owning function
								else if (localVariables.TryGetValue(currentValue, out HashSet<string> declaredInFunctions) && 
										 currentFunc != null && declaredInFunctions.Contains(currentFunc))
								{
									finalTokens.Add(new Token(TokenType.UserVariable, currentValue, currentToken.Start, currentToken.Length));
								}
								else
								{
									// Unknown variable declaration or not in its owning function, keep as UserVariable
									finalTokens.Add(new Token(TokenType.UserVariable, currentValue, currentToken.Start, currentToken.Length));
								}
							}
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
			
			if (hashIndex == -1)
				yield break; // Should not happen if StartsWith("#")

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
