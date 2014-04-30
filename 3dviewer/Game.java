import java.io.*;
import java.util.*;

import org.lwjgl.Sys;
import org.lwjgl.input.Keyboard;
import org.lwjgl.opengl.Display;
import org.lwjgl.opengl.DisplayMode;
import org.lwjgl.opengl.GL11;
import org.lwjgl.opengl.ARBFragmentShader;
import org.lwjgl.opengl.ARBShaderObjects;
import org.lwjgl.opengl.ARBVertexShader;
import org.lwjgl.util.glu.GLU;
import org.lwjgl.input.*;
import org.lwjgl.BufferUtils;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

class GameUpdater implements Runnable
{
    private LifeGame game;
    private boolean copyFinished;

    public GameUpdater(LifeGame g)
    {
        game = g;
        copyFinished = false;
    }

    @Override
    public void run()
    {
        game.update();
    }

    public boolean isCopyFinished()
    {
        return copyFinished;
    }

    public void setCopyFinished(boolean v)
    {
        copyFinished = v;
    }
}

class Face
{
    public static final int boxLength = 50;

    public static Point toGridCoords(float x, float y, float z)
    {
        int x_ = (int)Math.floor(x / boxLength);
        int y_ = (int)Math.floor(y / boxLength);
        int z_ = (int)Math.floor(z / boxLength);

        Point p_ = new Point(x_, y_, z_);

        return p_;
    }

    private Point[] vertices;
    private float r, g, b;

    public Face(Point[] vertices, float r, float g, float b)
    {
        this.vertices = vertices;
        this.r = r;
        this.g = g;
        this.b = b;
    }

    public void renderFace()
    {
        GL11.glColor3f(r, g, b);
        GL11.glBegin(GL11.GL_POLYGON);
            GL11.glVertex3i(vertices[3].x * boxLength, vertices[3].y * boxLength, vertices[3].z * boxLength);
            GL11.glVertex3i(vertices[2].x * boxLength, vertices[2].y * boxLength, vertices[2].z * boxLength);
            GL11.glVertex3i(vertices[1].x * boxLength, vertices[1].y * boxLength, vertices[1].z * boxLength);
            GL11.glVertex3i(vertices[0].x * boxLength, vertices[0].y * boxLength, vertices[0].z * boxLength);
        GL11.glEnd();
    }

    public void renderBorder()
    {
        GL11.glColor3f(0.2f, 0.2f, 0.2f);
        GL11.glBegin(GL11.GL_LINE_LOOP);
            GL11.glColor3f(0.2f, 0.2f, 0.2f);
            for(int i = 0; i < vertices.length; i++)
                GL11.glVertex3i(vertices[i].x * boxLength, vertices[i].y * boxLength, vertices[i].z * boxLength);
        GL11.glEnd();
    }

    public void move(Point distance)
    {
        for(int i = 0; i < vertices.length; i++)
            vertices[i] = vertices[i].add(distance);
    }

    @Override 
    public int hashCode()
    {
        int ret = 1;
        for(Point p : vertices)
            ret *= p.hashCode();
        return ret;
    }

    @Override
    public boolean equals(Object f_)
    {
        Face f = (Face)f_;

        List<Point> thisP = Arrays.asList(vertices);
        List<Point> thoseP = Arrays.asList(f.vertices);

        for(Point p : thisP)
            if(!thoseP.contains(p))
                return false;

        for(Point p : thoseP)
            if(!thisP.contains(p))
                return false;

        return true;
    }

    public String toString()
    {
        StringBuilder sb = new StringBuilder();

        sb.append(vertices[0].toString());

        for(int i = 1; i < vertices.length; i++)
        {
            sb.append('-');
            sb.append(vertices[i].toString());
        }

        return sb.toString();
    }
}

interface GameMode
{
    public void updateGame();
    public void update();
    public void render();
}

abstract class GameManager implements GameMode
{
	public static final float lineWidth = 2.0f;

	private static final Point[][] boxOffsets = 
    { // these are offsets to apply to a point XYZ to make a box
		{new Point(0, -1, -1), new Point(1, -1, -1), new Point(1, -1, 0),  new Point(0, -1, 0)}, 
		{new Point(1, 0, 0),   new Point(1, 0, -1),  new Point(1, -1, -1), new Point(1, -1, 0)},
		{new Point(0, 0, -1),  new Point(1, 0, -1),  new Point(1, 0, 0),   new Point(0, 0, 0)},
		{new Point(0, 0, -1),  new Point(1, 0, -1),  new Point(1, -1, -1), new Point(0, -1, -1)},
		{new Point(0, 0, 0),   new Point(0, 0, -1),  new Point(0, -1, -1), new Point(0, -1, 0)},
		{new Point(0, 0, 0),   new Point(1, 0, 0),   new Point(1, -1, 0),  new Point(0, -1, 0)}
	};

	public static void renderBox(Point p, float r, float g, float b)
	{
        Face[] faces = getBoxFacesFrom(p, r, g, b);

        for(Face face : faces)
        {
            face.renderFace();
            face.renderBorder();
        }
	}

    /** Produces an array of faces that arise from creating a cube originating at a given point.
     * @param p The origin of the cube to be generated.
     * @return A two-dimensional array of points whose size is exactly 6x4.
     */
    public static Face[] getBoxFacesFrom(Point p, float r, float g, float b)
    {
        Face[] faces = new Face[6];

        for(int i = 0; i < boxOffsets.length; i++)
        {
            Point[] vertices = new Point[4];
            for(int j = 0; j < boxOffsets[i].length; j++)
                vertices[j] = p.add(boxOffsets[i][j]);
            faces[i] = new Face(vertices, r, g, b);
        }

        return faces;
    }

    /** Batch render a bunch of cells by using the optimization of not rendering twice overlapping cell walls.
     * The color of the cell is determined by its age, whereby age is inversely proportional to redness and greenness.
     * This abstract class provides this method as static rather than as instance, else this class would be required to decide on a Cell management scheme, whereas such 
     * a decision should be left up to the derived class.
     * @param cs The cells to render. Can be null if a_faces is non-null and non-empty.
     * @param a_faces The faces of the cells to render. If null, then cs must be non-null, and the faces will be computed from cs and returned.
     * @return The set of faces produced. This value should be stored somewhere and invalidated when the configuration of cells changes. 
     */
    public static HashSet<Face> renderCells(Collection<Cell> cs, HashSet<Face> a_faces)
    {
        HashSet<Face> faces = a_faces == null ? new HashSet<Face>() : a_faces;

        if(faces.size() == 0)
            for(Cell c : cs)
                for(Face face : getBoxFacesFrom(c.position, 1f / c.age, 1f / c.age, 1f))
                    faces.add(face);

        for(Face face : faces)
        {
            face.renderFace();
            face.renderBorder();
        }

        return faces;
    }

    public static HashSet<Face> makeCellFaces(Collection<Cell> cs)
    {
        HashSet<Face> faces = new HashSet<Face>();

        for(Cell c : cs)
            for(Face face : getBoxFacesFrom(c.position, 1f / c.age, 1f / c.age, 1f))
                faces.add(face);

        return faces;
    }

    abstract public Point getDimensions();
    abstract public Point getOrigin();
    abstract public LifeGame getGame();
    abstract public void updateGame();
    abstract public void update();
    abstract public void render();
}

class RecordedGameManager extends GameManager
{
    private int index;
    private List<LifeGame> subgames;
    private List<HashSet<Face>> facesList;

    private static List<HashSet<Face>> makeFaceList(ArrayList<ArrayList<Cell>> states)
    {
        ArrayList<HashSet<Face>> faces = new ArrayList<HashSet<Face>>();

        for(Collection<Cell> cs : states)
            faces.add(makeCellFaces(cs));

        return faces;
    }

    public RecordedGameManager(ArrayList<ArrayList<Cell>> a_states)
    {
        index = 0;

        facesList = makeFaceList(a_states);

        subgames = new ArrayList<LifeGame>();

        System.err.printf("%d sets of faces read in to RecordedGameManager.\n", facesList.size());

        for(List<Cell> cs : a_states)
            subgames.add(new LifeGame(cs));
    }

    public RecordedGameManager(ArrayList<ArrayList<Cell>> a_states, int a_index)
    {
        index = a_index;

        facesList = makeFaceList(a_states);

        subgames = new ArrayList<LifeGame>();

        for(List<Cell> cs : a_states)
            subgames.add(new LifeGame(cs));
    }

    public LifeGame getGame()
    {
        return subgames.get(index);
    }

    public boolean isFinished()
    {
        return index == subgames.size() - 1;
    }

    @Override
    public Point getDimensions()
    {
        return subgames.get(index).getDimensions();
    }

    @Override 
    public Point getOrigin()
    {
        return subgames.get(index).getOrigin();
    }

    @Override
    public void updateGame()
    {
        if(isFinished())
            return;

        index++;
        System.err.printf("Moving on to frame %d in recording.\n", index + 1);
    }

    @Override
    public void update()
    {
        // no-op ?
    }

    @Override
    public void render()
    {
        for(Face f : facesList.get(index))
        {
            f.renderFace();
            f.renderBorder();
        }
    }
}

class LiveGameManager extends GameManager
{

    private LifeGame game;
    private Collection<Cell> cells;
    private HashSet<Face> facesMemoList;
    private GameUpdater updater;
    private Thread updater_t;

	public LiveGameManager(LifeGame initial)
	{
		game = initial;

        cells = game.getCellDump();
        updater = new GameUpdater(game);
        updater.setCopyFinished(true);
        updater_t = new Thread(updater);
        
        facesMemoList = makeCellFaces(cells);
        adjustFacePositions();
	}

    @Override
    public Point getDimensions() 
    {
        return game.getDimensions();
    }

    @Override
    public Point getOrigin()
    {
        return game.getOrigin();
    }

    @Override
    public LifeGame getGame()
    {
        return game;
    }

    @Override
    public void updateGame()
    {
        if(updater_t.isAlive() || !updater.isCopyFinished())
            return;

        updater.setCopyFinished(false);
        updater_t = new Thread(updater);
        updater_t.start();
    }

    @Override
    public void update()
    {
        if(updater_t.isAlive() || updater.isCopyFinished())
            return;

        cells = game.getCellDump();
        facesMemoList = makeCellFaces(cells);
        adjustFacePositions();

        updater.setCopyFinished(true);
    }

    /** Add a new Cell to the underlying LifeGame object. 
     * @param p The (absolute, i.e. relative to (0, 0, 0)) coordinates of the cell to be created.
     */
    public void addNewCell(Point p)
    {
        if(updater_t.isAlive() || !updater.isCopyFinished())
        {
            System.err.printf("Can't add cell while update is in progress!\n");
            return;
        }

        game.addCell(new Cell(game.rel(p)));
        cells = game.getCellDump();
        facesMemoList = makeCellFaces(cells);
        adjustFacePositions();
    }

    public void removeCell(Point p)
    {
        if(updater_t.isAlive() || !updater.isCopyFinished())
        {
            System.err.printf("Can't remove cell while update is in progress!\n");
            return;
        }

        game.removeCellAt(game.rel(p));
        cells = game.getCellDump();
        facesMemoList = makeCellFaces(cells);
        adjustFacePositions();
    }

    @Override
    public void render()
    {
        for(Face f : facesMemoList)
        {
            f.renderFace();
            f.renderBorder();
        }
    }

    private void adjustFacePositions()
    {
        //for(Face f : facesMemoList)
        //    f.move(game.getOrigin());
    }
}

interface Viewer
{
    
}
 
public class Game // TODO split up this class some more. As it is ~600 lines of code, it ressembles too much a God Object, and is too much like an OOP-antipattern
{
    public static final String GAME_TITLE = "Game of Life 3D by Alexandre Laporte & Jacob Errington";
 
    /** The framerate of the display. At 60 I get weird graphical artifacts with certain structures, so I've lowered it to 40, where these aren't visible. */
    private static final int FRAMERATE = 40;

    /** The number of window frames that must go by before the next game frame is displayed in autoupdate mode. */
    private static final int autoUpdateRate = 45; // number of frames before the game is updated, if autoupdate is enabled (otherwise updating is done by pressing space)

    private static boolean autoUpdateMode = false; // TODO all this stuff about modes should be in separate, related classes that alter/enhance the default behaviour
	private static boolean spinMode       = false; // Whether spin mode is active. A flashy feature that centers the lookat point on the centre of the configuration and spins the camera around it.
    private static boolean editMode       = false; // Whether the edit mode block cursor is visible and blocks can be added to the current configuration by clicking.
    private static boolean recording = false, playing = false; // whether the current sequence of configurations is being recorded to a file, and whether the current sequence of configurations is being played from a file.
    private static boolean showHUD = true; // Whether or not a heads-up display is visible. TODO get HUD to work!

    /** How far away the block cursor is in edit mode. */
    private static float   editArmLength  = 4 * Face.boxLength;

    /** How quickly the camera spins when in spin mode, in radians. */
	private static float   spinSpeed      = 0.15f; // TODO get spin mode to work!

    /** Represents the number of literal, window frames that have gone by.
     * Used to determine when to go to the next game frame (i.e. configuration of cells.)
     */
    private static long frameN = 0;
 
    // whether or not execution is over. (Program closes when set to true.)
    private static boolean finished;
 
    /** The maximum (default) speed of the viewer when moving is equal to one block per frame. */
    public static final float maxSpeed = Face.boxLength;

    /** The minimum speed, which is accessed by holding down shift and moving, is equal to one fifth of a block per frame. */
    public static final float minSpeed = Face.boxLength / 5f;

    /** The parameters of the viewer, such as its location in the world and its camera angle. 
     * Radius is a special internal value used to compute the location of the observation point.
     * The observation point is guaranteed to be radius distance away from the viewer location.
     * Making the radius larger will make camera panning smoother, but slower.
     * Theta is the angle of the camera in the x-z plane, whereas phi is how far up and down the camera is pointing.
     */
    private static float eyeX   = 0, 
                         eyeY   = 0, 
                         eyeZ   = 0, 
                         theta  = (float)Math.PI, //looking backwards initially. 
                         phi    = 0,
                         radius = 200,
                         speed  = maxSpeed; 

    /** Dimensions of the screen. */
    private static int w = 1024, h = 600;

    /** Used to compute changes to theta and phi based on mouse movement. */
    private static int lastMouseX = 0, lastMouseY = 0;

    /** The location (in grid coordinates) of the block cursor, which is visible only in edit mode. */
    private static Point cellCursor = new Point();

    /** The custom vertex and fragment shader programs. (UNUSED) */
    private static int program = 0;

    /** The internal game manager, which generally wraps one or more LifeGame objects and makes them renderable. */
    private static GameManager gm = null;

    /** The file chooser dialog box used to open and save patterns and recordings. */
    private static JFileChooser fc = new JFileChooser();

    /**
     * Application init
     * @param args Commandline args
     */
    public static void main(String[] args) 
    {
        try 
        {
            for(int i = 0; i < args.length; i++)
            {
                if(args[i].equals("-")) // allow specifying a file on the command line. If that file is '-', then allow reading from stdin
                {
                    if(gm == null)
                        gm = new LiveGameManager(LifeGame.fromFile(System.in));
                    else
                        JOptionPane.showMessageDialog(null, "Input data already specified. Ignoring subsequent specification.", "Error", JOptionPane.ERROR_MESSAGE);
                }
                else if(args[i].equals("-random"))
				{
                    int n = Integer.parseInt(args[i + 1]);
                    int range = Integer.parseInt(args[i + 2]);

                    gm = new LiveGameManager(LifeGame.fromRandom(n, range));
                    i += 2;
				}
                else if(args[i].equals("-recording"))
                {
                    if(args[i + 1].equals("-"))
                    {
                        gm = new RecordedGameManager(LifeGame.fromRecording(System.in));
                        playing = true;
                    }
                    else
                    {
                        try
                        {
                            FileInputStream fis = new FileInputStream(args[i + 1]);

                            if(gm == null)
                            {
                                gm = new RecordedGameManager(LifeGame.fromRecording(fis));
                                playing = true;
                            }
                            else
                                JOptionPane.showMessageDialog(null, "Input data already specified. Ignoring subsequent specification.", "Error", JOptionPane.ERROR_MESSAGE);

                        }
                        catch(FileNotFoundException e)
                        {
                            JOptionPane.showMessageDialog(null, "Recording file not found.", "Fatal Error", JOptionPane.ERROR_MESSAGE);
                            System.exit(1);
                        }
                    }

                    i++;
                }
                else if(args[i].equals("-rect-prism"))
                {
                    int x = Integer.parseInt(args[i + 1]);
                    int y = Integer.parseInt(args[i + 2]);
                    int z = Integer.parseInt(args[i + 3]);

                    gm = new LiveGameManager(LifeGame.fromPrism(x, y, z));

                    i += 3;
                }
                else
                {
                    try
                    {
                        if(gm == null)
                            gm = new LiveGameManager(LifeGame.fromFile(new FileInputStream(args[0])));
                        else
                            JOptionPane.showMessageDialog(null, "Input data already specified. Ignoring subsequence specification.", "Error", JOptionPane.ERROR_MESSAGE);
                    }
                    catch(FileNotFoundException e)
                    {
                        JOptionPane.showMessageDialog(null, 
                                                      "File not found: " + e.getMessage(), 
                                                      "Fatal Error", 
                                                      JOptionPane.ERROR_MESSAGE);
                        System.exit(1);
                    }
                }
            }


            if(gm == null) // if no file is specified on the command line, then open an empty game. The user can load a pattern or recording later with keyboard commands.
                gm = new LiveGameManager(new LifeGame());

            init();
            run();
        } 
        catch (Exception e) 
        {
            e.printStackTrace(System.err);
            JOptionPane.showMessageDialog(null, 
                                          "A fatal error occured and the program will exit.", 
                                          "Fatal Error", 
                                          JOptionPane.ERROR_MESSAGE);
        } 
        finally 
        {
            cleanup();
        }

        System.exit(0);
    }
 
    /**
     * Initialise the game
     * @throws Exception if init fails
     */
    private static void init() throws Exception 
    {
        try
        {
            Display.setDisplayMode(new DisplayMode(w, h));
            Display.setVSyncEnabled(true);
            Display.setTitle(GAME_TITLE);
            Display.create();
            Mouse.create();
            Cursor emptyCursor = new Cursor(1, 1, 0, 0, 1, BufferUtils.createIntBuffer(1), null);
            Mouse.setNativeCursor(emptyCursor);
            Mouse.setCursorPosition(w / 2, h / 2);
            lastMouseX = w / 2;
            lastMouseY = h / 2;
        }
        catch(Exception e)
        {
            System.err.println("Error setting up display.");
            System.exit(1);
        }

        int vert = 0, frag = 0;

        try // set up the shaders
        {
            vert = createShader("screen.vert", ARBVertexShader.GL_VERTEX_SHADER_ARB);
            frag = createShader("screen.frag", ARBFragmentShader.GL_FRAGMENT_SHADER_ARB);

            program = ARBShaderObjects.glCreateProgramObjectARB(); // this could fail for some reason

            ARBShaderObjects.glAttachObjectARB(program, vert);
            ARBShaderObjects.glAttachObjectARB(program, frag);

            ARBShaderObjects.glLinkProgramARB(program);
            ARBShaderObjects.glValidateProgramARB(program);
        }
        catch(Exception e)
        {
            System.err.printf("Error setting up shaders: %s", e.getMessage());
            System.exit(1);
        }

        GL11.glViewport(0,0,w,h);
        GL11.glMatrixMode(GL11.GL_PROJECTION);
        GL11.glLoadIdentity();
        GLU.gluPerspective(67.0f, ((float)w/(float)h),1f,10000.0f);
        GL11.glMatrixMode(GL11.GL_MODELVIEW);
        GL11.glLoadIdentity();
        GL11.glShadeModel(GL11.GL_SMOOTH);
        GL11.glClearDepth(10000f);
        GL11.glEnable(GL11.GL_DEPTH_TEST);
		GL11.glDepthMask(true);
        GL11.glDepthFunc(GL11.GL_LEQUAL);
		GL11.glDepthRange(0f, 1f);
        GL11.glHint(GL11.GL_PERSPECTIVE_CORRECTION_HINT, GL11.GL_NICEST);

		//ARBShaderObjects.glUseProgramObjectARB(program);
        System.err.println("init complete.");
    }
 
    /**
     * Runs the game (the "main loop")
     */
    private static void run() 
    {
 
        while (!finished) 
        {
            // Always call Window.update(), all the time - it does some behind the
            // scenes work, and also displays the rendered output
            Display.update();
 
            // Check for close requests
            if (Display.isCloseRequested()) 
            {
                finished = true;
            } 
 
            // The window is in the foreground, so we should play the game
            else if (Display.isActive()) 
            {
                logic();
                render();
                Display.sync(FRAMERATE);
            } 
 
            // The window is not in the foreground, so we can allow other stuff to run and
            // infrequently update
            else 
            {
                try 
                {
                    Thread.sleep(100);
                } 
                catch (InterruptedException e) 
                {
                }

                logic();
 
	// Only bother rendering if the window is visible or dirty
                if (Display.isVisible() || Display.isDirty()) 
                {
                    render();
                }
            }
        }
    }
 
    /**
     * Do any game-specific cleanup
     */
    private static void cleanup() 
    {
        // Close the window
        Display.destroy();
    }
 
    /**
     * Do all calculations, handle input, etc.
     */
    private static void logic()  // TODO refactor into something less that 250LOC !
    {
        if (Keyboard.isKeyDown(Keyboard.KEY_ESCAPE)) 
        {
            finished = true;
            return;
        }

        while(Keyboard.next())
        {
            int key = Keyboard.getEventKey();
            boolean state = Keyboard.getEventKeyState();
            char keyChar = Keyboard.getEventCharacter();
            int retv; // used by the filechooser

            if (!state) // key was released
			{
                switch (key)
                {
                    case Keyboard.KEY_F:
						retv = fc.showOpenDialog(null);

						if(retv == JFileChooser.APPROVE_OPTION)
						{
							try
							{
								FileInputStream fis = new FileInputStream(fc.getSelectedFile());
								gm = new LiveGameManager(LifeGame.fromFile(fis));
								autoUpdateMode = false;
							}
							catch(FileNotFoundException e)
							{
								JOptionPane.showMessageDialog(null, "File not found: " + e.getMessage(), "Fatal Error", JOptionPane.ERROR_MESSAGE);
							}
						}
						break;
                    case Keyboard.KEY_R:
						retv = fc.showOpenDialog(null);

						if(retv == JFileChooser.APPROVE_OPTION)
						{
							try
							{
								FileInputStream fis = new FileInputStream(fc.getSelectedFile());
								gm = new RecordedGameManager(LifeGame.fromRecording(fis));
								autoUpdateMode = false;
                                playing = true;
							}
							catch(FileNotFoundException e)
							{
								JOptionPane.showMessageDialog(null, "File not found: " + e.getMessage(), "Fatal Error", JOptionPane.ERROR_MESSAGE);
							}
						}
						break;
					case Keyboard.KEY_P:
						autoUpdateMode = !autoUpdateMode;
						break;
                    case Keyboard.KEY_B:
                        if(!playing)
                        {
                            editMode = !editMode;
                            //autoUpdateMode = false; // pause the game if it's running?
                            System.err.printf("Edit mode: %s\n", new Boolean(editMode).toString());
                        }
                        break;
					case Keyboard.KEY_SPACE:
						gm.updateGame();
						break;
                    case Keyboard.KEY_LSHIFT:
                        speed = maxSpeed;
                        break;
                    case Keyboard.KEY_BACK: // clear game
                        playing = false;
                        autoUpdateMode = false;
                        gm = new LiveGameManager(new LifeGame());
                        break;
                    case Keyboard.KEY_X: // a debug functionality
                        System.err.print(gm.getGame().toString());
                        break;
                    case Keyboard.KEY_RETURN: // save game to file
						retv = fc.showSaveDialog(null);

						if(retv == JFileChooser.APPROVE_OPTION)
						{
							try
							{
                                PrintStream ps = new PrintStream(fc.getSelectedFile());
                                ps.print(gm.getGame().toString());
                                ps.close();
							}
							catch(FileNotFoundException e)
							{
								JOptionPane.showMessageDialog(null, "File not found: " + e.getMessage(), "Fatal Error", JOptionPane.ERROR_MESSAGE);
							}
						}
                        break;
                    case Keyboard.KEY_1:
                        if(Keyboard.isKeyDown(Keyboard.KEY_LCONTROL)) // shift means save
                        {
                            try
                            {
                                PrintStream ps = new PrintStream("save_1.txt");
                                ps.print(gm.getGame().toString());
                                ps.close();
                            }
                            catch(FileNotFoundException e)
                            {
                            }
                        }
                        else // loading a state
                        {
                            playing = false;
                            autoUpdateMode = false;

                            try
                            {
                                gm = new LiveGameManager(LifeGame.fromFile(new FileInputStream("save_1.txt")));
                            }
                            catch(FileNotFoundException e)
                            {
								JOptionPane.showMessageDialog(null, "Could not open temporary save file save_1.txt.", "Fatal Error", JOptionPane.ERROR_MESSAGE);
                            }
                        }
                        break;
                    case Keyboard.KEY_ESCAPE:
                        finished = true;
                        return;
                    case Keyboard.KEY_N: // bail out of a recording, into interactive mode
                        if(playing)
                        {
                            playing = false;
                            autoUpdateMode = false;
                            gm = new LiveGameManager(gm.getGame());
                        }
                        break;
				}
			}
            else
            {
                switch (key)
                {
                    case Keyboard.KEY_LSHIFT:
                        speed = minSpeed;
                        break;
                }
            }
		}

        while(Mouse.next())
        {
            boolean buttonstate = Mouse.getEventButtonState();
            int     mousebutton = Mouse.getEventButton();

            if(!buttonstate) // button is released
            {
                switch(mousebutton)
                {
                    case 0: // left !
                        if(editMode && !playing)
                            ((LiveGameManager)gm).addNewCell(cellCursor);
                        break;
                    case 1: // right !
                        if(editMode && !playing)
                            ((LiveGameManager)gm).removeCell(cellCursor);
                        break;
                    case 2: // centre !
                        //System.err.printf("Mouse button 2 pressed.\n");
                        break;
                }
            }
        }

		if(spinMode)
		{
			theta += (float)Math.PI * spinSpeed / FRAMERATE;
		}
		else
		{
			if(Display.isActive())
			{
				int mx = Mouse.getX(), my = Mouse.getY();
				int mdx = mx - lastMouseX, mdy = my - lastMouseY;

				theta += -mdx / radius;
				phi   += mdy / radius;

				if(mdx != 0.0f)
				{
					if(theta > (float)Math.PI * 2f)
						theta -= (float)Math.PI * 2f;
					else if(theta < 0f)
						theta += (float)Math.PI * 2f;

                    //System.err.printf("Theta = %f\n", theta);
				}
                if(mdy != 0.0f)
                {
                    //System.err.printf("Phi = %f\n", phi);
                }
			}

			if (Keyboard.isKeyDown(Keyboard.KEY_Q))
				eyeY -= speed;
            else if (Keyboard.isKeyDown(Keyboard.KEY_E))
				eyeY += speed;

            // Side to side movement is just forwards/backwards movement with the trig functions switched
			if (Keyboard.isKeyDown(Keyboard.KEY_A))
			{
				eyeZ += speed * Math.cos(theta + Math.PI / 2.0);
				eyeX += speed * Math.sin(theta + Math.PI / 2.0);
			}
			if (Keyboard.isKeyDown(Keyboard.KEY_D))
			{
				eyeZ -= speed * Math.cos(theta + Math.PI / 2.0);
				eyeX -= speed * Math.sin(theta + Math.PI / 2.0);
			}
			if (Keyboard.isKeyDown(Keyboard.KEY_W))
			{
				eyeZ += speed * Math.cos(theta);
				eyeX += speed * Math.sin(theta);
				eyeY += speed * Math.sin(phi);
			}
			if (Keyboard.isKeyDown(Keyboard.KEY_S))
			{
				eyeZ -= speed * Math.cos(theta);
				eyeX -= speed * Math.sin(theta);
				eyeY -= speed * Math.sin(phi);
			}
		}

		if(Display.isActive()) // Only centre the mouse if the window is focused!
			Mouse.setCursorPosition(w / 2, h / 2);

        if(phi > (float)Math.PI / 2) // just making sure phi isn't allowed to go round and round
            phi = (float)Math.PI / 2f;
        else if(phi < -(float)Math.PI / 2f)
            phi = -(float)Math.PI / 2f;

        cellCursor = Face.toGridCoords(eyeX + editArmLength * (float)Math.sin(theta), eyeY + editArmLength * (float)Math.sin(phi), eyeZ + editArmLength * (float)Math.cos(theta));

        if (autoUpdateMode)
        {
            if(frameN % autoUpdateRate == 0)
                gm.updateGame();
        }

		frameN++;

        gm.update();
    }
 
    /**
     * Render the current frame
     */
    private static void render() 
    {
        // clear the screen and add depth buffering to avoid awkward overlapping of surfaces
        GL11.glClear(GL11.GL_DEPTH_BUFFER_BIT | GL11.GL_COLOR_BUFFER_BIT | GL11.GL_STENCIL_BUFFER_BIT);
		GL11.glClearDepth(10000f);

        GL11.glLoadIdentity();

		if(spinMode)
		{
            Point dim = gm.getDimensions();
            Point origin = gm.getOrigin();

			float structCentreX = ((float)origin.x + (float)dim.x / 2f) * Face.boxLength;
			float structCentreY = ((float)origin.y + (float)dim.y / 2f) * Face.boxLength;
			float structCentreZ = ((float)origin.z + (float)dim.z / 2f) * Face.boxLength;

			float distX = eyeX - structCentreX;
			float distY = eyeY - structCentreY;
			float distZ = eyeZ - structCentreZ;
			
			float structDistance = (float)Math.sqrt(distX * distX + distY * distY + distZ * distZ);
			Point structCentre = new Point();
			GLU.gluLookAt(structDistance * (float)Math.cos(theta), eyeY, structDistance * (float)Math.sin(theta), 
						  structCentreX, structCentreY, structCentreZ,
						  0, 1, 0);
		}
		else
			GLU.gluLookAt(eyeX, eyeY, eyeZ, eyeX + radius * (float)Math.sin(theta), eyeY + radius * (float)Math.sin(phi), eyeZ + radius * (float)Math.cos(theta), 0, 1, 0);
 
        if(editMode)
            GameManager.renderBox(cellCursor, 0.5f, 1.0f, 0.5f);

        gm.render();

        GL11.glPushMatrix();

        if(showHUD) // TODO get HUD to work.
        {
            GL11.glLoadIdentity();
            GL11.glOrtho(-300, 300, -300, 300, 100, -100);

            GL11.glBegin(GL11.GL_LINES);
                GL11.glVertex3f(-200, -200, 5f);
                GL11.glVertex3f(200, 200, 5f);
            GL11.glEnd();
        }
 
        GL11.glPopMatrix();
    }

    /**
    * With the exception of syntax, setting up vertex and fragment shaders
    * is the same.
    * @param the name and path to the vertex shader
    */
    private static int createShader(String filename, int shaderType) throws Exception 
    {
    	int shader = 0;
    	try 
        {
	        shader = ARBShaderObjects.glCreateShaderObjectARB(shaderType);
	        
	        if(shader == 0)
	        	return 0;
	        
	        ARBShaderObjects.glShaderSourceARB(shader, readFileAsString(filename));
	        ARBShaderObjects.glCompileShaderARB(shader);
	        
	        if (ARBShaderObjects.glGetObjectParameteriARB(shader, ARBShaderObjects.GL_OBJECT_COMPILE_STATUS_ARB) == GL11.GL_FALSE)
	            throw new RuntimeException("Error creating shader: " + getLogInfo(shader));
	        
	        return shader;
    	}
    	catch(Exception exc) {
    		ARBShaderObjects.glDeleteObjectARB(shader);
    		throw exc;
    	}
    }
    
    private static String getLogInfo(int obj) 
    {
        return ARBShaderObjects.glGetInfoLogARB(obj, ARBShaderObjects.glGetObjectParameteriARB(obj, ARBShaderObjects.GL_OBJECT_INFO_LOG_LENGTH_ARB));
    }
    
    private static String readFileAsString(String filename) throws Exception 
    {
        StringBuilder source = new StringBuilder();
        
        FileInputStream in = new FileInputStream(filename);
        
        Exception exception = null;
        
        BufferedReader reader;
        try
        {
            reader = new BufferedReader(new InputStreamReader(in,"UTF-8"));
            
            Exception innerExc= null;
            try 
            {
            	String line;
                while((line = reader.readLine()) != null)
                    source.append(line).append('\n');
            }
            catch(Exception exc) 
            {
            	exception = exc;
            }
            finally 
            {
            	try 
                {
            		reader.close();
            	}
            	catch(Exception exc) 
                {
            		if(innerExc == null)
            			innerExc = exc;
            		else
            			exc.printStackTrace();
            	}
            }
            
            if(innerExc != null)
            	throw innerExc;
        }
        catch(Exception exc) 
        {
        	exception = exc;
        }
        finally 
        {
        	try 
            {
        		in.close();
        	}
        	catch(Exception exc) 
            {
        		if(exception == null)
        			exception = exc;
        		else
					exc.printStackTrace();
        	}
        	
        	if(exception != null)
        		throw exception;
        }
        
        return source.toString();
    }
}
