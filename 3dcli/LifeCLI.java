import java.io.*;
import java.util.*;

class LifeGameManager
{
    LifeGame game;

    public LifeGameManager()
    {
        game = new LifeGame();
    }

    public LifeGameManager(LifeGame initial)
    {
        game = initial;
    }

    /** The 'main loop' of the REPL. */
    public void run()
    {
        boolean finished = false;

        Scanner stdinScanner = new Scanner(System.in);
        Scanner lineScanner;

        String commandline;
        ArrayList<String> line = new ArrayList<String>();

        while(!finished)
        {
            System.out.printf("life> ");
            commandline = stdinScanner.nextLine();

            lineScanner = new Scanner(commandline);
            line.clear();

            while(lineScanner.hasNext())
                line.add(lineScanner.next());

            if(line.size() == 0)
                continue;

            if(line.get(0).equals("c") || line.get(0).equals("create"))
            {
                // These coordinates are absolute, i.e. relative to (0, 0, 0)
                int x = Integer.parseInt(line.get(1)),
                    y = Integer.parseInt(line.get(2)),
                    z = Integer.parseInt(line.get(3)),
                    age = line.size() == 5 ?
                            Integer.parseInt(line.get(4)) : 1;

                // We must relativize the coordinates to the floating origin
                Point relPosition = game.rel(new Point(x, y, z));
                game.addCell(new Cell(relPosition, age));
            }
            else if(line.get(0).equals("x") || line.get(0).equals("remove"))
            {
                int x = Integer.parseInt(line.get(1)),
                    y = Integer.parseInt(line.get(2)),
                    z = Integer.parseInt(line.get(3));

                Point relPosition = game.rel(new Point(x, y, z));
                game.removeCellAt(relPosition);
            }
            else if(line.get(0).equals("erase"))
            {
                game = new LifeGame();
            }
            else if(line.get(0).equals("w"))
            {
                String outPath = line.get(1);

                try
                {
                    PrintWriter pw = new PrintWriter(outPath);
                    pw.print(game.toString());
                    pw.close();
                }
                catch(FileNotFoundException e)
                {
                    System.err.printf("Error - File not found: %s\n", e.getMessage());
                }
            }
            else if(line.get(0).equals("r") || line.get(0).equals("read"))
            {
                String inPath = line.get(1);

                try
                {
                    FileInputStream fis = new FileInputStream(inPath);
                    game = LifeGame.fromFile(fis);
                    fis.close();
                }
                catch(FileNotFoundException e)
                {
                    System.err.printf("Error - File not found: %s\n", e.getMessage());
                }
                catch(IOException e)
                { // TODO maybe add some kind of message here
                }
            }
            else if(line.get(0).equals("u") || line.get(0).equals("update"))
            {
                game.update();
            }
            else if(line.get(0).equals("p") || line.get(0).equals("print"))
            {
                System.out.print(game.toString());
            }
            else if(line.get(0).equals("q") || line.get(0).equals("quit"))
            {
                finished = true;
            }
            else
            {
                System.err.printf("Unknown command: %s.\n", line.get(0));
            }
        }
    }
}

public class LifeCLI
{
    public static void main(String[] args)
    {
        InputStream in = null;
        LifeGameManager man;

        try
        {
            for(int i = 0; i < args.length; i++)
            {
                if(args[i].equals("-i"))
                {
                    String inputFilePath = args[i + 1];

                    try
                    {
                        in = new FileInputStream(inputFilePath);
                    }
                    catch(FileNotFoundException e)
                    {
                        System.err.printf("File not found: %s.\n", e.getMessage());
                        System.exit(1);
                    }
                }
            }
        }
        catch(ArrayIndexOutOfBoundsException e)
        {
            System.err.printf("Unexpected end of command line arguments.\n");
            System.exit(1);
        }

        man = new LifeGameManager(in == null ? new LifeGame() : LifeGame.fromFile(in));

        man.run();
    }
}
