import java.io.*;
import java.util.*;

public class LifeRecorder
{
    public static void main(String[] args)
    {
        int framesToRender = 100;
        LifeGame game = null;

        InputStream in = System.in;
        PrintStream out = System.out;

        try
        {
            for(int i = 0; i < args.length; i++)
            {
                // -i and -o options are provided for n00bz using Windows
                if(args[i].equals("-i")) // check if the current argument is a switch for input file
                {
                    String inputFilePath = args[i + 1]; // if so, then the next argument is the path to the input file

                    try
                    {
                        in.close(); // so we close standard input, which was previously assigned to `in`
                        in = new FileInputStream(inputFilePath); // and we open a FileInputStream to the path given in the i + 1-th argument
                    }
                    catch(FileNotFoundException e) // and we catch any filenotfound exceptions.
                    {
                        System.err.printf("File not found: %s\n", inputFilePath);
                        System.exit(1); // if such an error occurs, we simply abort the program.
                    }

                    i++;
                    continue;
                } // if no -i switch is given, then `in` will simply have the value of System.in

                if(args[i].equals("-o"))
                {
                    String outputFilePath = args[i + 1];

                    try
                    {
                        out.close();
                        out = new PrintStream(outputFilePath);
                    }
                    catch(FileNotFoundException e)
                    {
                        System.err.printf("File not found: %s\n", outputFilePath);
                        System.exit(1);
                    }

                    i++;
                    continue;
                }

                if(args[i].equals("-n"))
                {
                    framesToRender = Integer.parseInt(args[i + 1]);
                    i++;
                    continue;
                }

                if(args[i].equals("-r")) // resume a previous recording from the last frame rendered
                {
                    FileInputStream fis = new FileInputStream(args[i + 1]);
                    List<LifeGame> states = LifeGame.fromRecording(fis);
                    fis.close();
                    game = states.get(states.size() - 1); // get the last state
                    i++;
                    continue;
                }
            }
        }
        catch(ArrayIndexOutOfBoundsException e)
        {
            System.err.printf("Unexpected end of command line arguments: %s\n", e.getMessage());
            System.exit(1);
        }
        catch(NumberFormatException e)
        {
            System.err.printf("Invalid number of frames to render: %s\n", e.getMessage());
            System.exit(1);
        }
        catch(IOException e)
        {
            System.err.printf("IO exception: %s\n", e.getMessage());
            System.exit(1);
        }

        if(framesToRender < 1)
        {
            System.err.printf("Invalid number of frames to render: %s\n", framesToRender);
            System.exit(1);
        }

        if(game == null)
            game = LifeGame.fromFile(in);

        out.printf("#1\n");
        out.printf("%s", game.toString());

        for(int i = 1; i < framesToRender && game.isAlive(); i++)
        {
            out.printf("---\n"); 
            out.printf("#%d\n", i + 1);
            game.update();
            out.printf("%s", game.toString()); // don't need an \n at the end of %s because tostring includes a newline

            System.err.printf("%.1f%% complete.\n", (float)i / (float)framesToRender * 100f);
        }

        System.err.printf("Done.\n");
    }
}
