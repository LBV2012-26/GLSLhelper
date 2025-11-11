using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Collections.Generic;
using System.Linq;

namespace GLSLhelper.Test
{
	[TestClass()]
	public class GlslTokenizerTests
	{
		[DataTestMethod()]
		[DynamicData(nameof(GetSingleTokenData), DynamicDataSourceType.Method)]
		public void TokenizeSingleTest(string text, TokenType expectedType)
		{
			var tokenizer = new GlslParser();
			var tokens = tokenizer.Tokenize(text);
			Assert.AreEqual(1, tokens.Count());
			var token = tokens.First();
			Assert.AreEqual(expectedType, token.Type);
			Assert.AreEqual(text, text.Substring(token.Start, token.Length));
		}

		[DataTestMethod()]
		[DynamicData(nameof(GetTokensData), DynamicDataSourceType.Method)]
		public void TokenizeTest(string text, TokenType[] expectedTypes)
		{
			var tokenizer = new GlslParser();
			var tokens = tokenizer.Tokenize(text);
			var types = tokens.Select(token => token.Type).ToArray();

			static string Print<Type>(IEnumerable<Type> types) => string.Join(',', types);
			CollectionAssert.AreEqual(types, expectedTypes, $"\n\tExpected={Print(expectedTypes)}\n\tActual={Print(types)}");
		}

		private static IEnumerable<object[]> GetSingleTokenData()
		{
			yield return new object[] { "// comment stuff", TokenType.Comment };
			yield return new object[] { "/* comment stuff\n\n # preprocessor */", TokenType.Comment };
			yield return new object[] { "max", TokenType.Function };
			yield return new object[] { "a", TokenType.Identifier };
			yield return new object[] { "_x123", TokenType.Identifier };
			yield return new object[] { "vec4", TokenType.Keyword };
			yield return new object[] { "1", TokenType.Number };
			yield return new object[] { ".1", TokenType.Number };
			yield return new object[] { "1.", TokenType.Number };
			yield return new object[] { "#warning", TokenType.Preprocessor };
			yield return new object[] { "gl_FragCoord", TokenType.BuiltInVariable };
			yield return new object[] { "gl_RayTmaxEXT", TokenType.BuiltInVariable };
			yield return new object[] { "shadercallcoherent", TokenType.Keyword };
			yield return new object[] { "rayQueryGetIntersectionWorldToObjectEXT", TokenType.Function };
			yield return new object[] { "u16vec4", TokenType.Keyword };
			yield return new object[] { "f16mat4x3", TokenType.Keyword };
			yield return new object[] { "float16_t", TokenType.Keyword };
			yield return new object[] { "float32_t", TokenType.Keyword };
			yield return new object[] { "f32mat4x4", TokenType.Keyword };
			yield return new object[] { "float64_t", TokenType.Keyword };
			yield return new object[] { "u8vec4", TokenType.Keyword };
			foreach (var op in GlslSpecification.Operators)
			{
				yield return new object[] { op.ToString(), TokenType.Operator };
			}
		}

		private static IEnumerable<object[]> GetTokensData()
		{
			yield return new object[] { "#define @hallo", new TokenType[] { TokenType.Preprocessor, TokenType.Identifier } };
			yield return new object[] { "//#include \"test 12.5 \\ / * löä \"\n //comment", new TokenType[] { TokenType.Comment, TokenType.Comment } };
			yield return new object[] { "//#include \"test 12.5 \\ / * löä \" //comment", new TokenType[] { TokenType.Comment } };
			yield return new object[] { "#include \"test 12.5 \\ / * löä \" //comment", new TokenType[] { TokenType.Preprocessor, TokenType.QuotedString, TokenType.Comment } };
			yield return new object[] { "#include \"test\" //comment", new TokenType[] { TokenType.Preprocessor, TokenType.QuotedString, TokenType.Comment } };
			yield return new object[] { "#include \"test\n //comment", new TokenType[] { TokenType.Preprocessor, TokenType.QuotedString, TokenType.Comment } };
			yield return new object[] { "#include \"test\r\n //comment", new TokenType[] { TokenType.Preprocessor, TokenType.QuotedString, TokenType.Comment } };
			yield return new object[] { "#include \"test\n\"\" //comment", new TokenType[] { TokenType.Preprocessor, TokenType.QuotedString, TokenType.QuotedString, TokenType.Comment } };
			yield return new object[] { "#version 330\\//comment", new TokenType[] { TokenType.Preprocessor, TokenType.Number, TokenType.Operator, TokenType.Comment } };
			yield return new object[] { "#version 330 \\	  //comment", new TokenType[] { TokenType.Preprocessor, TokenType.Number, TokenType.Operator, TokenType.Comment } };
			yield return new object[] { "// comment stuff\n#prepor", new TokenType[] { TokenType.Comment, TokenType.Preprocessor } };
			yield return new object[] { "#prepor\r\n/* comment stuff uniform float;\n\n*/", new TokenType[] { TokenType.Preprocessor, TokenType.Comment } };
			yield return new object[] { "#version\n uniform float test", new TokenType[] { TokenType.Preprocessor, TokenType.Keyword, TokenType.Keyword, TokenType.Identifier } };
			yield return new object[] { "\"", new TokenType[] { TokenType.QuotedString } };
			yield return new object[] { "R\"(\nvoid main\n)\"", new TokenType[] { TokenType.Identifier, TokenType.QuotedString, TokenType.Keyword, TokenType.Identifier, TokenType.Operator, TokenType.QuotedString } };
			yield return new object[] { "gl_FragCoord = vec4(1.)", new TokenType[] { TokenType.BuiltInVariable, TokenType.Operator, TokenType.Keyword, TokenType.Operator, TokenType.Number, TokenType.Operator } };
		}
	}
}