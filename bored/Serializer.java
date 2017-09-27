import java.util.ArrayList;
import java.util.List;

public class Serializer {
  private static final String TOKENIZATION_DELIMITER = " ";
  private static final String SERIALIZATION_DELIMITER = "-";

  public static void main(String[] args) {
    String s = args[0];
    String serialized = serialize(s);
    String deserialized = deserialize(serialized);
    System.out.println(serialized);
    System.out.println(deserialized);
  }

  /** 
   * Given a {@link String}, returns a serialized version of the {@link String}.
   * 
   * <p>Serialization appends a header which has the total number of tokens
   * as its first value, a delimiter, then the lengths of each token, each
   * seperated by a delimiter.<br>
   * e.g.: "hello, world" -> "2-6-5-hello,world"
   */
  private static String serialize(String input) {
    String[] tokens = input.split(TOKENIZATION_DELIMITER);
    String serialized = ""; // Should use StringBuilder if expecting large input.
    int numTokens = tokens.length;
    String header = numTokens + SERIALIZATION_DELIMITER;
    for(String token : tokens) {
      header += token.length() + SERIALIZATION_DELIMITER;
      serialized += token;
    } 
    return header + serialized;
  }

  /** 
   * Given a {@link String} serialized with {@link #serialize}, returns the original
   * {@link String}. 
   *
   * @retun the deserialized {@link String}. Note that if the String contains extra characters
   * at the end, they will be ignored:<br>
   * e.g.: "2-6-5-hello,worldIGNORED_CHARS" -> "hello, world"
   */
  private static String deserialize(String input) {
    // This is probably of of the dumbest ways to do this. Escaping would work much better.
    int numTokens = Integer.parseInt(input.charAt(0) + "");
    List<Integer> tokenSizes = new ArrayList<>()
    for (int i = 1; i < numTokens; i++) {
        
      tokens.add(Integer.parseInt());
    }
    String serialized = tokens.get(tokens.size()-1);

    if (tokens.size() < numTokens) {
      throw new IllegalArgumentException("The given String is incorrectly serialized.");
    }

    String deserialized = ""; // Should use StringBuilder if expecting large input.
    int nextTokenPtr = 0;
    for (int i = 1; i < numTokens; i++) {
      int tokenLength = Integer.parseInt(tokens.get(i));
      deserialized += serialized.substring(nextTokenPtr, nextTokenPtr + tokenLength) + TOKENIZATION_DELIMITER;
      nextTokenPtr += tokenLength;
    }
    return deserialized.substring(0, deserialized.length()-1); // Remove extra " " at the end of the String. 
  }
}
