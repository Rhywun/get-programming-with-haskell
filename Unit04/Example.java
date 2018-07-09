public class Example
{
    // This is a pure function
    public static int mystery1(int val1, int val2)
    {
        int val3 = 3;
        return Math.pow(val1 + val2 + val3, 2);
    }

    // This function is impure
    public static int mystery2(int val1, int val2)
    {
         int val3 = 3;
         System.out.print("Enter a number");
         try
         {
             Scanner in = new Scanner(System.in);
             val3 = in.nextInt();
         }
         catch (IOException e)
         {
             e.printStackTrace();
         }
         return Math.pow(val1 + val2 + val3, 2);
     }
}
