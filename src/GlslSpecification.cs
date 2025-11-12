using System.Collections.Generic;

namespace GLSLhelper
{
	public static partial class GlslSpecification
	{
		private static readonly (Dictionary<string, TokenType> reservedWords,
								 HashSet<string> builtInTypes,
								 HashSet<string> preprocessorDirectives,
								 HashSet<string> compoundTypes) _spec = Initialize();
		
		private static readonly Dictionary<string, TokenType> _reservedWords = _spec.reservedWords;
		public static readonly HashSet<string> BuiltInTypes                  = _spec.builtInTypes;
		public static readonly HashSet<string> PreprocessorDirectives        = _spec.preprocessorDirectives;
		public static readonly HashSet<string> CompoundTypes                 = _spec.compoundTypes;

		public const string Operators = "~.;,+-*/()[]{}<>=&$!%?:|^\\";

		public static IEnumerable<KeyValuePair<string, TokenType>> ReservedWords => _reservedWords;

		public static TokenType GetReservedWordType(string word)
		{
			if (_reservedWords.TryGetValue(word, out var type))
				return type;
			return TokenType.Identifier;
		}

		public static bool IsBuiltInType(string word)
		{
			return BuiltInTypes.Contains(word);
		}

		public static bool IsPreprocessorDirective(string word)
		{
			return PreprocessorDirectives.Contains(word);
		}

		public static bool IsIdentifierChar(char c) => char.IsDigit(c) || IsIdentifierStartChar(c);

		public static bool IsIdentifierStartChar(char c) => char.IsLetter(c) || '_' == c || '@' == c;

		private static void AddRange(this Dictionary<string, TokenType> result, IEnumerable<string> words, TokenType type)
		{
			foreach (var word in words)
			{
				result[word] = type;
			}
		}
	}
}