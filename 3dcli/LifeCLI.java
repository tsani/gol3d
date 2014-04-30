import java.io.*;
import java.util.*;

class LifeGameManager
{
    LifeGame game;
    private boolean interactive;
    private InputStream inputSource;

    public LifeGameManager(boolean interactive_)
    {
        game = new LifeGame();
        inputSource = System.in;
    }

    public LifeGameManager(LifeGame initial, InputStream src, boolean interactive_)
    {
        game = initial;
        inputSource = src;
        interactive = interactive_;
    }

    /** The 'main loop' of the REPL. */
    public void run()
    {
        boolean finished = false;
        int exitCode = 0;

        Scanner stdinScanner = new Scanner(inputSource);
        Scanner lineScanner;

        String commandline = "";
        ArrayList<String> line = new ArrayList<String>();

        while(!finished)
        {
            if(interactive)
                System.out.printf("life> ");

            if(stdinScanner.hasNext())
                commandline = stdinScanner.nextLine();
            else
                System.exit(exitCode);

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

                exitCode = 0;
            }
            else if(line.get(0).equals("x") || line.get(0).equals("remove"))
            {
                int x = Integer.parseInt(line.get(1)),
                    y = Integer.parseInt(line.get(2)),
                    z = Integer.parseInt(line.get(3));

                Point relPosition = game.rel(new Point(x, y, z));
                game.removeCellAt(relPosition);

                exitCode = 0;
            }
            else if(line.get(0).equals("erase"))
            {
                game = new LifeGame();

                exitCode = 0;
            }
            else if(line.get(0).equals("w"))
            {
                String outPath = line.get(1);

                try
                {
                    PrintWriter pw = new PrintWriter(outPath);
                    pw.print(game.toString());
                    pw.close();
                    exitCode = 0;
                }
                catch(FileNotFoundException e)
                {
                    System.err.printf("Error - File not found: %s\n", e.getMessage());
                    exitCode = 1;
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
                    exitCode = 0;
                }
                catch(FileNotFoundException e)
                {
                    System.err.printf("Error - File not found: %s\n", e.getMessage());
                    exitCode = 1;
                }
                catch(IOException e)
                { // TODO maybe add some kind of message here
                    exitCode = 1;
                }
            }
            else if(line.get(0).equals("u") || line.get(0).equals("update"))
            {
                game.update();
                exitCode = 0;
            }
            else if(line.get(0).equals("test"))
            {
                if(line.size() < 2)
                {
                    System.err.printf("Please specify at least one test to perform.\n");
                    exitCode = 1;
                    continue;
                }

                int n = 100;

                if(line.size() == 3)
                    n = Integer.parseInt(line.get(2));

                if(line.get(1).equals("stable"))
                    exitCode = LifeGame.isStable(game, n) ? 0 : 1;
                else if(line.get(1).equals("cycle"))
                    exitCode = LifeGame.isCyclical(game, n) ? 0 : 1;
                else
                {
                    System.err.printf("Unknown test '%s'.\n", line.get(1));
                    exitCode = 1;
                    continue;
                }

                if(interactive)
                    System.out.println(exitCode == 0 ? "Succeeded" : "Failed");
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
        InputStream gameIn = null;
        InputStream scriptIn = System.in;
        LifeGameManager man;
        boolean interactive = true;

        try
        {
            for(int i = 0; i < args.length; i++)
            {
                if(args[i].equals("-i"))
                {
                    String inputFilePath = args[i + 1];

                    try
                    {
                        gameIn = new FileInputStream(inputFilePath);
                    }
                    catch(FileNotFoundException e)
                    {
                        System.err.printf("File not found: %s.\n", e.getMessage());
                        System.exit(1);
                    }
                }

                if(args[i].equals("-s"))
                {
                    interactive = false;

                    if(args[i+1].equals("-"))
                        scriptIn = System.in;
                    else
                        try
                        {
                            scriptIn = new FileInputStream(args[i+1]);
                        }
                        catch(FileNotFoundException e)
                        {
                            System.err.printf("File not found: %s\n", args[i+1]);
                            System.exit(2);
                        }
                }
            }
        }
        catch(ArrayIndexOutOfBoundsException e)
        {
            System.err.printf("Unexpected end of command line arguments.\n");
            System.exit(2);
        }

        man = new LifeGameManager(gameIn == null ? new LifeGame() : LifeGame.fromFile(gameIn), scriptIn, true);

        man.run();
    }
}
