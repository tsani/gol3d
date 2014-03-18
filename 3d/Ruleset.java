public interface Ruleset
{
    public Iterable<Point> getNeighbours(Point centre);
    public SurvivalResult getResultFor(int n);
}
