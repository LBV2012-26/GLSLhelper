using System.Text.RegularExpressions;

namespace GLSLhelper
{
	public static class ShaderTypeDetector
	{
		private static readonly Regex _tessellationControlVertices = new Regex(@"layout\s*\(\s*vertices");

		/// <summary>
		/// Tries to detect the type of shader from keywords inside the shader code.
		/// </summary>
		/// <param name="shaderCode">GLSL source code</param>
		/// <returns>Will detect a <see cref="ShaderType.Fragment"/> if no other shader type matches.</returns>
		public static ShaderType AutoDetectFromCode(string shaderCode)
		{
			if (shaderCode.Contains("EmitVertex"))
				return ShaderType.Geometry;
			if (shaderCode.Contains("local_size_"))
				return ShaderType.Compute;
			if (_tessellationControlVertices.IsMatch(shaderCode))
				return ShaderType.TessellationControl;
			if (shaderCode.Contains("gl_TessCoord"))
				return ShaderType.TessellationEvaluation;
			if (shaderCode.Contains("gl_Position"))
				return ShaderType.Vertex;
			return ShaderType.Fragment;
		}
	}
}
