import java.util.*;
import java.io.*;

public class GameOfLife
{
	public static void main (String[] args)
	{
		int iterations = 100;

        LifeGame g = null;

        try
        {
            g = LifeGame.fromFile(System.in);
        }
        catch(NoSuchElementException e)
        {
            System.err.println("Could not parse file.");
            return;
        }

        if(args.length > 0)
        {
            int duration = 100;
            if(args.length == 2)
                duration = Integer.parseInt(args[1]);

            if(args[0].equals("-test-stable"))
            {
                if(LifeGame.isStable(g, duration))
                    System.out.printf("stable\n");
                else
                    System.out.printf("unstabe\n");
            }
            else if(args[0].equals("-test-cycle"))
            {
                if(LifeGame.isCyclical(g, duration))
                    System.out.printf("stable\n");
                else
                    System.out.printf("unstabe\n");
            }
            else
            {
                System.out.printf("Unsupported test `%s`.\n", args[0]);
                System.exit(1);
            }
            System.out.printf("Iterations required: %d\n", g.getN());
        }
        else
        {
            System.out.println("Initial state:");
            System.out.println(g.toString());
            for(int i = 0; i < iterations; i++)
            {
                g.update();
                //System.out.println(g.toString());
                g.get();
            }
        }
	}
}
