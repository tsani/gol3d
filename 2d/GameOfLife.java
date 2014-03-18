import java.util.*;
import java.io.*;

class Point extends Object
{
    public Point(int a_x, int a_y)
    {
        x = a_x;
        y = a_y;
    }

    public Point()
    {
        x = 0;
        y = 0;
    }

    public Point add(Point p)
    {
        return new Point(x + p.x, y + p.y);
    }

    public Point neg()
    {
        return new Point(-x, -y);
    }

    public Point subtract(Point p)
    {
        return add(p.neg());
    }

    //// Comparison is performed by distance to the origin
    //@Override
    //public int compareTo(Point p)
    //{
    //    if(equals(p))
    //        return 0;

    //    if(x + y > p.x + p.y)
    //        return 1;
    //    else
    //        return -1;
    //}

    /** Returns the greatest Point P such that adding P only to positive points will produce both this point or p.
     */
    public Point min(Point p)
    {
        return new Point(Math.min(x, p.x), Math.min(y, p.y));
    }

    public Point max(Point p)
    {
        return new Point(Math.max(x, p.x), Math.max(y, p.y));
    }

    public String toString()
    {
        return String.format("%d %d", x, y);
    }

    @Override
    public boolean equals(Object p_)
    {
        Point p = (Point)p_;
        return x == p.x && y == p.y;
    }

    @Override
    public int hashCode()
    {
        return (int)(Math.round(Math.pow(2, x) * Math.pow(3, y)));
    }

    //public boolean lt(Point p)
    //{
    //    if(x < p.x)
    //        return y <= p.y;
    //    else if(y < p.y)
    //        return x <= p.x;
    //    else
    //        return false;
    //}

    //public boolean gt(Point p)
    //{
    //    if(x > p.x)
    //        return y >= p.y;
    //    else if(y > p.y)
    //        return x >= p.x;
    //    else
    //        return false;
    //}

    public final int x;
    public final int y;
}

class Cell
{
    public Cell(boolean a_alive, Point a_position)
    {
        alive = a_alive;
        position = a_position;
        age = 1;
    }

    public Cell(Point a_position)
    {
        position = a_position;
        alive = true;
        age = 1;
    }

    public Point[] getNeighbours()
    {
        Point[] ns = {
                new Point(position.x - 1, position.y - 1),
                new Point(position.x, position.y - 1),
                new Point(position.x + 1, position.y - 1),
                new Point(position.x - 1, position.y),
                new Point(position.x + 1, position.y),
                new Point(position.x - 1, position.y + 1),
                new Point(position.x, position.y + 1),
                new Point(position.x + 1, position.y + 1)
            };

        return ns;
    }

    public boolean alive;
    public Point position;
    public int age;
}

class Game
{
    HashMap<Point, Cell> state;
    Point topLeft, botRight; // represents the bounding box of the game (changes between calls to update)

    public static Game fromFile(InputStream data) throws FileNotFoundException
    {
        ArrayList<Cell> cs = new ArrayList<Cell>();

        Scanner fscan;

        fscan = new Scanner(data);

        while(fscan.hasNextLine())
        {
            int x, y;

            String line = fscan.nextLine();

            if(line.length() == 0) // empty line support
                continue;
            else if(line.charAt(0) == ';' || line.charAt(0) == '#') // comment support.
                continue;

            Scanner lscan = new Scanner(line);

            // these calls may throw. The caller must take care of this.
            x = lscan.nextInt();
            y = lscan.nextInt();

            cs.add(new Cell(new Point(x, y)));
        }

        System.err.printf("%d cells read from file.\n", cs.size());

        return new Game(cs);
    }

    public Game(List<Cell> cs)
    {
        topLeft = cs.get(0).position;
        botRight = cs.get(0).position;

        state = new HashMap<Point, Cell>();

        for (Cell c : cs)
        {
            state.put(c.position, c);

            topLeft = topLeft.min(c.position);
            botRight = botRight.max(c.position);
        }

        adjustBounds();

        System.err.printf("Topleft at (%s)\n", topLeft);
    }

    public boolean update()
    {
        HashMap<Point, Integer> neighbours = new HashMap<Point, Integer>();

        HashMap<Point, Cell> nextState = new HashMap<Point, Cell>();

        for(Cell cell : state.values()) // iterate over the cells to construct the hashmap of neighbour-counts.
        {
            for(Point n : cell.getNeighbours())
            {
                Integer v = neighbours.get(n);

                if(v != null)
                    v = new Integer(v.intValue() + 1);
                else
                    v = new Integer(1);

                neighbours.put(n, v);
            }
        }

        // iterate over the neighbour-counts to determine the new layout of cells
        for(Map.Entry<Point, Integer> p : neighbours.entrySet())
        {
            Cell c;
            //System.err.printf("Neighbouring cells at (%s): %d\n", p.getKey().toString(), p.getValue());
            switch(p.getValue().intValue())
            {
                case 0:
                case 1:
                    // the cell simply dies of underpopulation.
                    break;
                case 2:
                    c = state.get(p.getKey());

                    if(c != null)
                    {
                        nextState.put(p.getKey(), c);
                    }

                    break;
                case 3:
                    c = state.get(p.getKey());

                    if(c == null)
                        c = new Cell(p.getKey());

                    nextState.put(p.getKey(), c);
                    break;
                default:
                    // The cell dies of overcrowding.
                    break;
            }
        }

        System.err.printf("New origin (%s)\n", topLeft.toString());

        state = nextState; // we replace the old state with the new one.

        return state.size() != 0;
    }

    public void update(int n)
    {
        for(int i = 0; i < n; i++)
            update();
        adjustBounds();
    }

    // redetermines the value of topleft and adjusts all cells accordingly
    // Complexity: O(n)
    public void adjustBounds()
    {
        Point mn = null, mx = null;

        for(Cell c : state.values())
        {
            Point cp = abs(c.position);

            if(mn == null)
                mn = cp;
            else
                mn = mn.min(cp);

            if(mx == null)
                mx = cp;
            else
                mx = mx.max(cp);
        }

        botRight = mx;

        HashMap<Point, Cell> newState = new HashMap<Point, Cell>();

        for(Cell c : state.values())
        {
            //System.err.printf("Adjusting position of cell from (%s) ", c.position.toString());
            c.position = abs(c.position).subtract(mn);
            //System.err.printf("to (%s).\n", c.position.toString());
            newState.put(c.position, c);
        }

        topLeft = mn;

        //System.err.printf("Adjusted origin to (%s).\n", topLeft.toString());

        state = newState;
    }


    public int[][] get()
    {
        int[][] grid;

        Point dim = botRight.subtract(topLeft).add(new Point(1, 1));

        grid = new int[dim.x][dim.y]; // the initial grid is filled with zeroes.

        System.err.printf("Created grid with dimensions (%d,%d)\n", dim.x, dim.y);

        for(Cell c : state.values())
        {
            Point p = (c.position);
            grid[p.x][p.y] = c.age;
        }

        return grid;
    }

    public String toString()
    {
        String s = "";

        for(Cell c : state.values())
        {
            s += c.position.toString();
            s += "\n";
        }

        return s;
    }

    public String render(boolean withBox)
    {
        int[][] grid = get();

        String s = "+";

        for(int i = 0; i < grid.length; i++)
            s += '-';
        s += "+\n";

        for(int i = 0; i < grid[0].length; i++)
        {
            s += '|';
            for(int j = 0; j < grid.length; j++)
            {
                if(grid[j][i] > 0)
                    s += '#';
                else 
                    s += ' ';
            }
            s += '|';
            s += '\n';
        }

        s += '+';
        for(int i = 0; i < grid.length; i++)
            s += '-';
        s += '+';

        return s;
    }

    public String render()
    {
        return render(true);
    }

    public Point rel(Point p)
    {
        return p.subtract(topLeft);
    }

    public Point abs(Point p)
    {
        return p.add(topLeft);
    }

    public boolean equals(Game g)
    {
        if(g.state.size() != state.size())
            return false;

        for(Cell c : state.values())
        {
            if(g.state.get(g.abs(rel(c.position))) == null)
                return false;
        }

        return true;
    }
}

public class GameOfLife
{
	public static void main (String[] args)
	{
        Game g = null;

        try
        {
            g = Game.fromFile(System.in);
        }
        catch(FileNotFoundException e)
        {
            System.err.println("Could not file the layout file.");
            System.exit(1);
        }

        for(int i = 0;;i++)
        {
            System.out.println("Iteration #" + i);
            System.out.println(g.render());
            g.update();
            g.adjustBounds();
            try
            {
                Thread.sleep(100);
            }
            catch(Exception e)
            {
            }
            //if (i % 10 == 0)
            //   g.adjustBounds();
        }
	}
}
