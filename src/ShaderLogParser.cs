using System;
using System.Collections.Generic;
using System.Text.RegularExpressions;

namespace GLSLhelper
{
	/// <summary>
	/// Class that parses GLSL shader logs into lines with line numbers
	/// </summary>
	public class ShaderLogParser
	{
		/// <summary>
		/// Initializes a new instance of the <see cref="ShaderLogParser"/> class by parsing the given log string.
		/// </summary>
		/// <param name="log">The log.</param>
		public ShaderLogParser(string log)
		{
			//parse error log
			log = log.Replace("\r", string.Empty);
			var lines = log.Split(new char[] { '\n' }, StringSplitOptions.RemoveEmptyEntries);
			var errorLines = new List<ShaderLogLine>();
			var otherLines = new List<ShaderLogLine>();
			foreach (var line in lines)
			{
				var logLine = ParseLogLineNvidia(line);
				if (logLine is null)
				{
					logLine = ParseGlslc(line);
					if (logLine is null)
					{
						logLine = ParseOthersLogLine(line);
					}
					if (logLine is null)
					{
						logLine = new ShaderLogLine { Message = $"Could not parse line '{line}'" };
					}
				}
				if (MessageType.Error == logLine.Type)
				{
					errorLines.Add(logLine);
				}
				else
				{
					otherLines.Add(logLine);
				}

			}
			//first error messages, then all others
			_lines = errorLines;
			_lines.AddRange(otherLines);
		}

		/// <summary>
		/// Gets the lines.
		/// </summary>
		/// <value>
		/// The lines.
		/// </value>
		public IEnumerable<ShaderLogLine> Lines => _lines;

		/// <summary>
		/// The lines
		/// </summary>
		private readonly List<ShaderLogLine> _lines = new List<ShaderLogLine>();

		//filename(10): error C0000: syntax error, unexpected '[', expecting \"::\" at token \"[\"
		private static readonly Regex _nvidiaLine = new Regex(@"(.+)\((\d+)\)\s?:\s(\w+)(.+)");

		/// <summary>
		/// Parses the log line NVIDIA.
		/// </summary>
		/// <param name="line">The line.</param>
		/// <returns></returns>
		/// <exception cref="ArgumentException"></exception>
		private static ShaderLogLine ParseLogLineNvidia(string line)
		{
			var match = _nvidiaLine.Match(line);
			if (match.Success && 5 == match.Groups.Count)
			{
				if (!int.TryParse(match.Groups[2].Value, out int lineNumber)) return null;
				return new ShaderLogLine
				{
					FileId     = match.Groups[1].Value,
					LineNumber = lineNumber,
					Type       = ParseType(match.Groups[3].Value),
					Message    = match.Groups[4].Value,
				};
			}

			return null;
		}

		//C:\work\IrrlichtBAW\branch\examples_tests\42.EnvmapLookup\envCubeMapShaders\envmap.frag:442: error: '=' :  cannot convert from ' const float' to ' temp highp uint'
		private static readonly Regex _glslcLine = new Regex(@"(.+):(\d+):\s(\w+):(.+)");

		private static ShaderLogLine ParseGlslc(string line)
		{
			var match = _glslcLine.Match(line);
			if (match.Success && 5 == match.Groups.Count)
			{
				if (!int.TryParse(match.Groups[2].Value, out int lineNumber)) return null;
				return new ShaderLogLine
				{
					FileId     = match.Groups[1].Value,
					LineNumber = lineNumber,
					Type       = ParseType(match.Groups[3].Value),
					Message    = match.Groups[4].Value,
				};
			}

			return null;
		}

		//ERROR: 0:9: '' :  syntax error, unexpected IDENTIFIER, expecting COMMA or SEMICOLON
		//private static readonly Regex othersLine = new Regex(@"(\w+):\s*(\d+):\s*(\d+):(.+)");
		private static readonly Regex _othersLine = new Regex(@"(\w+):\s*(.+):\s*(\d+):(.+)");

		/// <summary>
		/// Parses the log line.
		/// </summary>
		/// <param name="line">The line.</param>
		/// <returns></returns>
		/// <exception cref="ArgumentException"></exception>
		private static ShaderLogLine ParseOthersLogLine(string line)
		{
			var match = _othersLine.Match(line);
			if (match.Success && 5 == match.Groups.Count)
			{
				//if (!int.TryParse(match.Groups[2].Value, out int fileNumber)) return null;
				if (!int.TryParse(match.Groups[3].Value, out int lineNumber)) return null;
				return new ShaderLogLine
				{
					FileId     = match.Groups[2].Value,
					LineNumber = lineNumber,
					Type       = ParseType(match.Groups[1].Value),
					Message    = match.Groups[4].Value,
				};
			}

			return null;
		}

		/// <summary>
		/// Parses the type.
		/// </summary>
		/// <param name="typeString">The type string.</param>
		/// <returns></returns>
		private static MessageType ParseType(string typeString)
		{
			if (0 == typeString.Length)
				return MessageType.Message;
			typeString = char.ToUpper(typeString[0]) + typeString.Substring(1).ToLowerInvariant().Trim();
			if (Enum.TryParse<MessageType>(typeString, out var value))
			{
				return value;
			}
			else
			{
				return MessageType.Message;
			}
		}
	}
}
