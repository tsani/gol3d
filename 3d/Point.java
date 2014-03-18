public class Point
{
    public Point(int a_x, int a_y, int a_z)
    {
        x = a_x;
        y = a_y;
        z = a_z;
    }

    public Point()
    {
        x = 0;
        y = 0;
        z = 0;
    }

    public Point add(Point p)
    {
        Point q = new Point(x + p.x, y + p.y, z + p.z);
        //System.err.printf("(%s) + (%s) = (%s)\n", this.toString(), p.toString(), q.toString());
        return q;
    }

    public Point neg()
    {
        return new Point(-x, -y, -z);
    }

    public Point subtract(Point p)
    {
        return add(p.neg());
    }

    public float norm()
    {
		return (float)Math.sqrt(x * x + y * y + z * z);
	}

    /** Returns the greatest Point P such that adding P only to positive points will produce both this point or p.
     */
    public Point min(Point p)
    {
        return new Point(Math.min(x, p.x), Math.min(y, p.y), Math.min(z, p.z));
    }

    public Point max(Point p)
    {
        return new Point(Math.max(x, p.x), Math.max(y, p.y), Math.max(z, p.z));
    }

    public String toString()
    {
        return String.format("%d %d %d", x, y, z);
    }

    @Override
    public boolean equals(Object p_)
    {
        Point p = (Point)p_;
        return x == p.x && y == p.y && z == p.z;
    }

    @Override
    public int hashCode()
    {
        // TODO fix Godel numbering. It sucks
        return (int)(Math.round(Math.pow(2, x) * Math.pow(3, y) * Math.pow(5, z)));
    }

    public final int x;
    public final int y;
    public final int z;
}
