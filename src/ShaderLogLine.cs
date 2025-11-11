using System.Text;

namespace GLSLhelper
{
	/// <summary>
	/// Contains a parsed shader log line
	/// </summary>
	public class ShaderLogLine
	{
		/// <summary>
		/// The type
		/// </summary>
		public MessageType Type = MessageType.Message;

		/// <summary>
		/// The file id
		/// </summary>
		public string FileId = string.Empty;

		/// <summary>
		/// The line number
		/// </summary>
		public int? LineNumber = null;

		/// <summary>
		/// The message
		/// </summary>
		public string Message = string.Empty;

		/// <summary>
		/// Returns a <see cref="string" /> that represents this instance.
		/// </summary>
		/// <returns>
		/// A <see cref="string" /> that represents this instance.
		/// </returns>
		public override string ToString()
		{
			StringBuilder builder = new StringBuilder();
			if (MessageType.Message == Type)
			{
				builder.Append(Type + ": ");
			}
			if (LineNumber.HasValue)
			{
				builder.Append("Line ");
				builder.Append(LineNumber);
				builder.Append(" : ");
			}
			builder.Append(Message);
			return builder.ToString();
		}
	}
}
