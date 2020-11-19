/* This program will convert Roman numerals to ordinary arabic numerals
       and vice versa.  The user can enter a numerals of either type.  Arabic
       numerals must be in the range from 1 to 3999 inclusive.  The user ends
       the program by entering an empty line.
    */

public class RomanConverter {

    public static void main(String[] args) {

        TextIO.putln("Enter a Roman numeral and I will convert it to an ordinary");
        TextIO.putln("arabic integer.  Enter an integer in the range 1 to 3999");
        TextIO.putln("and I will convert it to a Roman numeral.  Press return when");
        TextIO.putln("you want to quit.");

        while (true) {

            TextIO.putln();
            TextIO.put("? ");

             /* Skip past any blanks at the beginning of the input line.
                Break out of the loop if there is nothing else on the line. */

            while (TextIO.peek() == ' ' || TextIO.peek() == '\t')
                TextIO.getAnyChar();
            if (TextIO.peek() == '\n')
                break;

             /* If the first non-blank character is a digit, read an arabic
                numeral and convert it to a Roman numeral.  Otherwise, read
                a Roman numeral and convert it to an arabic numeral. */

            if (Character.isDigit(TextIO.peek())) {
                int arabic = TextIO.getlnInt();
                try {
                    RomanNumeral N = new RomanNumeral(arabic);
                    TextIO.putln(N.toInt() + " = " + N.toString());
                } catch (NumberFormatException e) {
                    TextIO.putln("Invalid input.");
                    TextIO.putln(e.getMessage());
                }
            } else {
                String roman = TextIO.getln();
                try {
                    RomanNumeral N = new RomanNumeral(roman);
                    TextIO.putln(N.toString() + " = " + N.toInt());
                } catch (NumberFormatException e) {
                    TextIO.putln("Invalid input.");
                    TextIO.putln(e.getMessage());
                }
            }

        }  // end while

        TextIO.putln("OK.  Bye for now.");

    }  // end main()

} // end class RomanConverter