import java.io.*;
import java.util.*;

class GeneralCornerRuleset implements Ruleset
{
    private static Point[] neighbourOffsets = {
        new Point(0 - 1, 0 - 1, 0),
        new Point(0,     0 - 1, 0),
        new Point(0 + 1, 0 - 1, 0),
        new Point(0 - 1, 0,     0),
        new Point(0 + 1, 0,     0),
        new Point(0 - 1, 0 + 1, 0),
        new Point(0,     0 + 1, 0),
        new Point(0 + 1, 0 + 1, 0),

        new Point(0, 0 - 1, 0 - 1),
        new Point(0, 0 - 1, 0 + 1),
        new Point(0, 0,     0 - 1),
        new Point(0, 0,     0 + 1),
        new Point(0, 0 + 1, 0 - 1),
        new Point(0, 0 + 1, 0 + 1),

        new Point(0 - 1, 0, 0 - 1),
        new Point(0 + 1, 0, 0 - 1),
        new Point(0 - 1, 0, 0 + 1),
        new Point(0 + 1, 0, 0 + 1),
        
        new Point(0 - 1, 0 - 1, 0 - 1),
        new Point(0 + 1, 0 - 1, 0 - 1),
        new Point(0 + 1, 0 - 1, 0 + 1),
        new Point(0 - 1, 0 - 1, 0 + 1),
        new Point(0 - 1, 0 + 1, 0 - 1),
        new Point(0 - 1, 0 + 1, 0 + 1),
        new Point(0 + 1, 0 + 1, 0 - 1),
        new Point(0 + 1, 0 + 1, 0 + 1)
    };

    private int r1, r2, r3, r4;

    public GeneralCornerRuleset(int a_r1, int a_r2, int a_r3, int a_r4)
    {
        r1 = a_r1;
        r2 = a_r2;
        r3 = a_r3;
        r4 = a_r4;
    }

    @Override
    public Iterable<Point> getNeighbours(Point centre)
    {
        ArrayList<Point> neighbours = new ArrayList<Point>(neighbourOffsets.length);
        for(Point p : neighbourOffsets)
            neighbours.add(centre.add(p));
        return neighbours;
    }

    @Override
    public SurvivalResult getResultFor(int n)
    {
        if(n >= r1 && n <= r2)
            return SurvivalResult.BIRTH;
        else if(n > r3 || n < r4)
            return SurvivalResult.DEATH;
        else
            return SurvivalResult.SURVIVAL;
    }
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

    public void update()
    {
		age++;
    }

    public boolean alive;
    public Point position;
    public int age;
}

public class LifeGame
{
    public static LifeGame fromFile(InputStream data)
    {
        ArrayList<Cell> cs = new ArrayList<Cell>();

        Scanner fscan, lscan;

        fscan = new Scanner(data);

        int x, y, z;
        String line;

        while(fscan.hasNextLine())
        {
            line = fscan.nextLine();

            if(line.length() == 0) // empty line support
                continue;
            else if(line.charAt(0) == ';' || line.charAt(0) == '#') // comment support.
                continue;

            lscan = new Scanner(line);

            // these calls may throw. The caller must take care of this.
            x = lscan.nextInt();
            y = lscan.nextInt();
            z = lscan.nextInt();

            cs.add(new Cell(new Point(x, y, z)));
        }

        return new LifeGame(cs);
    }

    public static LifeGame fromRandom(int dataNumber, int range)
    {
		ArrayList<Cell> cs = new ArrayList<Cell>();
		Random r = new Random();

		for(int i = 0; i < dataNumber; i++)
		{
			cs.add(new Cell(new Point(r.nextInt(range) - range, r.nextInt(range) - range, r.nextInt(range) - range)));
		}

		return new LifeGame(cs);
	}

    public static LifeGame fromPrism(int x, int y, int z)
    {
        ArrayList<Cell> cs = new ArrayList<Cell>();

        for(int i = 0; i < x; i++)
            for(int j = 0; j < y; j++)
                for(int k = 0; k < z; k++)
                    cs.add(new Cell(new Point(i, j, k)));

        return new LifeGame(cs);
    }

    // We define stability to mean that the system breaks down into a fully unchanging system
    public static boolean isStable(LifeGame g, int threshold)
    {
        HashMap<Point, Cell> pastState = g.state;

        for(int i = 0; i < threshold; i++)
        {
            g.update();
            if(g.equals(pastState))
                return true;
            pastState = g.state;
        }
        
        return false;
    }

    // We define a cyclical system as one in which the current state has returned to the initial state, minus translations.
    public static boolean isCyclical(LifeGame g, int threshold)
    {
        HashMap<Point, Cell> initialState = g.state;

        for(int i = 0; i < threshold; i++)
        {
            g.update();
            if(g.equals(initialState))
                return true;
        }

        return false;
    }

    public static int[][][] copyGrid(int[][][] g)
    {
        int[][][] h = new int[g.length][g[0].length][g[0][0].length];

        for(int i = 0; i < g.length; i++)
            for(int j = 0; j < g[0].length; j++)
                for(int k = 0; k < g[0][0].length; k++)
                    h[i][j][k] = g[i][j][k];

        return h;
    }

    HashMap<Point, Cell> state;
    Point topLeft, botRight; // represents the bounding box of the game (changes between calls to update)
    int n; // iteration number
    Ruleset ruleset;

    public LifeGame(List<Cell> cs)
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

        ruleset = new GeneralCornerRuleset(6, 6, 7, 5);
    }

    public boolean isAlive()
    {
        return state.size() != 0;
    }

    public void update()
    {
        HashMap<Point, Integer> neighbours = new HashMap<Point, Integer>();

        HashMap<Point, Cell> nextState = new HashMap<Point, Cell>();

        for(Cell cell : state.values()) // iterate over the cells to construct the hashmap of neighbour-counts.
        {
            for(Point n : ruleset.getNeighbours(cell.position))
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
            int v = p.getValue().intValue();
            Cell c;

            switch(ruleset.getResultFor(v))
            {
                case DEATH:
                    break;
                case BIRTH:
                    c = state.get(p.getKey());

                    if(c == null)
                        c = new Cell(p.getKey());
                    else
                        c.update();

                    nextState.put(p.getKey(), c);
                    break;
                case SURVIVAL:
                    c = state.get(p.getKey());

                    if(c != null)
                    {
                        c.update();	
                        nextState.put(p.getKey(), c);
                    }
                    break;
            }
        }
                    
        state = nextState; // we replace the old state with the new one.

        if(state.size() != 0)
            adjustBounds();

        n++;
    }

    /** Update the state of the game a fixed number of times.
     * The floating origin is adjusted after the updates.
     * @param n The number of iterations to pass.
     */
    public void update(int n)
    {
        for(int i = 0; i < n; i++)
            update();
    }

    // redetermines the value of topleft and adjusts all cells accordingly
    // Complexity: O(n)
    private void adjustBounds()
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
            c.position = abs(c.position).subtract(mn);
            newState.put(c.position, c);
        }

        topLeft = mn;

        state = newState;
    }

    public Point getOrigin()
    {
        return topLeft;
    }

    public int[][][] get()
    {
        int[][][] grid;

        Point dim = botRight.subtract(topLeft).add(new Point(1, 1, 1));

        grid = new int[dim.x][dim.y][dim.z]; // the initial grid is filled with zeroes.

        for(Cell c : state.values())
        {
            Point p = c.position;
            grid[p.x][p.y][p.z] = c.age;
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

    public Point rel(Point p)
    {
        return p.subtract(topLeft);
    }

    public Point abs(Point p)
    {
        return p.add(topLeft);
    }

    public boolean equals(HashMap<Point, Cell> ht)
    {
        if(ht.size() != state.size())
            return false;

        for(Cell c : state.values())
        {
            if(!ht.containsKey(c.position))
                return false;
        }

        return true;
    }

    public int getN()
    {
        return n;
    }
}
