public class Cell extends Object
{
    public Cell(Point a_position)
    {
        position = a_position;
        age = 1;
    }

    public void update()
    {
		age++;
    }

    @Override
    public Cell clone()
    {
        Cell c = new Cell(position);
        c.age = age;
        return c;
    }

    @Override
    public boolean equals(Object c)
    {
        return position.equals(((Cell)c).position);
    }

    public Point position;
    public int age;
}

