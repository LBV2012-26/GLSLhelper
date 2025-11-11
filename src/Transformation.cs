using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;

namespace GLSLhelper
{
	public static class Transformation
	{
		/// <summary>
		/// Searches for #include statements in the given shader code and replaces them with the code returned by <paramref name="GetIncludeCode"/>.
		/// </summary>
		/// <param name="shaderCode">The shader code.</param>
		/// <param name="GetIncludeCode">Functor that will be called with the include path as parameter and returns the include code.</param>
		/// <returns>The expanded shader code.</returns>
		/// <exception cref="ArgumentNullException">GetIncludeCode</exception>
		public static string ExpandIncludes(string shaderCode, Func<string, string> GetIncludeCode)
		{
			if (GetIncludeCode == null)
			{
				throw new ArgumentNullException(nameof(GetIncludeCode));
			}

			var foundIncludes = new List<string>();
			int prevAmountOfIncludes;
			do {
				prevAmountOfIncludes = foundIncludes.Count;
				shaderCode = RemoveComments(UnixLineEndings(shaderCode));
				var lines  = shaderCode.Split(new[] { '\n' }, StringSplitOptions.None); //if UNIX style line endings still working so do not use Envirnoment.NewLine
				int lineNr = 1;
				foreach (var line in lines)
				{
					// Search for include pattern (e.g. #include raycast.glsl)
					var match = RegexPatterns.Include.Match(line);
					if (match.Success)
					{
						var sFullMatch  = match.Value;
						var includeName = match.Groups[1].ToString(); // get the include
																	  // Check for cyclic inclusion
						if (foundIncludes.Contains(includeName))
						{
							shaderCode = shaderCode.Replace(sFullMatch, string.Empty);
							continue;
						}

						var lineNumberCorrection = $"\n#line {lineNr}\n";
						var includeCode = GetIncludeCode(includeName);
						shaderCode = shaderCode.Replace(sFullMatch, includeCode + lineNumberCorrection); // replace #include with actual include code
						foundIncludes.Add(includeName);
					}
					++lineNr;
				}
			} while (prevAmountOfIncludes != foundIncludes.Count);

			return shaderCode;
		}

		public static string UnixLineEndings(string shaderCode) => shaderCode.Replace("\r\n", "\n");

		public static string ReplaceBlockCommentsByEmptyLines(string shaderCode)
		{
			int CountLineEndings(string text) => text.Count(c => c == '\n');
			
			while (true)
			{
				var match = RegexPatterns.BlockComment.Match(shaderCode);
				if (match.Success)
				{
					shaderCode = shaderCode.Replace(match.Value, new string('\n', CountLineEndings(match.Value)));
				}
				else
				{
					break;
				}
			}

			return shaderCode;
		}

		public static string RemoveLineComments(string shaderCode)
		{
			var pattern = "//.*"; //match everything till end of line
			return Regex.Replace(shaderCode, pattern, string.Empty);
		}

		public static string RemoveComments(string shaderCode)
		{
			return RemoveLineComments(ReplaceBlockCommentsByEmptyLines(shaderCode));
		}
	}
}
