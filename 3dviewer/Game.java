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

class GameManager
{
    public static final int boxLength = 50;
	public static final float lineWidth = 2.0f;

	public static final Point[][] boxOffsets = { // these are offsets to apply to a point XYZ to make a box
		{new Point(0, -1, -1), new Point(1, -1, -1), new Point(1, -1, 0),  new Point(0, -1, 0)}, 
		{new Point(1, 0, 0),   new Point(1, 0, -1),  new Point(1, -1, -1), new Point(1, -1, 0)},
		{new Point(0, 0, -1),  new Point(1, 0, -1), new Point(1, 0, 0), new Point(0, 0, 0)},
		{new Point(0, 0, -1),  new Point(1, 0, -1), new Point(1, -1, -1), new Point(0, -1, -1)},
		{new Point(0, 0, 0), new Point(0, 0, -1), new Point(0, -1, -1), new Point(0, -1, 0)},
		{new Point(0, 0, 0), new Point(1, 0, 0), new Point(1, -1, 0), new Point(0, -1, 0)}
	};

    private LifeGame game;
    private Collection<Cell> cells;
    private GameUpdater updater;
    private Thread updater_t;

	public GameManager(LifeGame initial)
	{
		game = initial;

        cells = game.getCellDump();
        updater = new GameUpdater(game);
        updater.setCopyFinished(true);
        updater_t = new Thread(updater);
	}

    public Point getDimensions() // TODO rather than have silly functions like this, we should probably make GameManager subclass LifeGame
    {
        return game.getDimensions();
    }

    public Point getOrigin()
    {
        return game.getOrigin();
    }

    public void updateGame()
    {
        if(updater_t.isAlive() || !updater.isCopyFinished())
            return;

        updater.setCopyFinished(false);
        updater_t = new Thread(updater);
        updater_t.start();
    }

    public void update()
    {
        if(updater_t.isAlive() || updater.isCopyFinished())
            return;

        cells = game.getCellDump();

        updater.setCopyFinished(true);
    }

    public void addNewCell(Point p)
    {
        if(updater_t.isAlive() || !updater.isCopyFinished())
        {
            System.err.printf("Can't add cell while update is in progress!\n");
            return;
        }

        game.addCell(new Cell(p.subtract(game.getOrigin())));
        cells = game.getCellDump();
    }

    public void removeCell(Point p)
    {
        if(updater_t.isAlive() || !updater.isCopyFinished())
        {
            System.err.printf("Can't remove cell while update is in progress!\n");
            return;
        }

        game.removeCellAt(p.subtract(game.getOrigin()));
        cells = game.getCellDump();
    }

	private static void renderQuad(Point v1, Point v2, Point v3, Point v4)
	{
		GL11.glVertex3i(v4.x * boxLength, v4.y * boxLength, v4.z * boxLength);
		GL11.glVertex3i(v3.x * boxLength, v3.y * boxLength, v3.z * boxLength);
		GL11.glVertex3i(v2.x * boxLength, v2.y * boxLength, v2.z * boxLength);
		GL11.glVertex3i(v1.x * boxLength, v1.y * boxLength, v1.z * boxLength);
	}

	private static void renderLine(Point v1, Point v2)
	{
		GL11.glVertex3i(v1.x * boxLength, v1.y * boxLength, v1.z * boxLength);
		GL11.glVertex3i(v2.x * boxLength, v2.y * boxLength, v2.z * boxLength);
	}

	public static void renderBox(Point p, float r, float g, float b)
	{
		GL11.glColor3f(r, g, b);
		GL11.glBegin(GL11.GL_QUADS);
			for(Point[] ps : boxOffsets) // this used to be a reversed for loop
				renderQuad(p.add(ps[0]), p.add(ps[1]), p.add(ps[2]), p.add(ps[3]));
		GL11.glEnd();

		GL11.glColor3f(0.2f, 0.2f, 0.2f);
		for(Point[] ps : boxOffsets)
		{
			GL11.glBegin(GL11.GL_LINES);
				renderLine(p.add(ps[0]), p.add(ps[1]));
				renderLine(p.add(ps[1]), p.add(ps[2]));
				renderLine(p.add(ps[2]), p.add(ps[3]));
				renderLine(p.add(ps[3]), p.add(ps[0]));
			GL11.glEnd();
		}
	}

    public Point toGridCoords(float x, float y, float z)
    {
        int x_ = (int)Math.floor(x / boxLength);
        int y_ = (int)Math.floor(y / boxLength);
        int z_ = (int)Math.floor(z / boxLength);

        Point p_ = new Point(x_, y_, z_);

        return p_;
    }

	private void renderCellBox(Point p, float age)
	{
        renderBox(p, 1.0f / age, 1.0f / age, 1.0f);
	}

    public void render()
    {
        for(Cell c : cells)
        {
            renderCellBox(game.getOrigin().add(c.position), c.age);
        }
    }
}
 
public class Game 
{
    /** Game title */
    public static final String GAME_TITLE = "Game of Life 3D by Alexandre Laporte & Jacob Errington";
 
    /** Desired frame time */
    private static final int FRAMERATE = 60;

    private static final int autoUpdateRate = 45; // number of frames before the game is updated, if autoupdate is enabled (otherwise updating is done by pressing space)

    private static boolean autoUpdateMode = false;
	private static boolean spinMode       = false;
    private static boolean editMode       = false;
    private static float   editArmLength  = 200f;
	private static float   spinSpeed      = 0.15f;

    private static long frameN = 0;
 
    /** Exit the game */
    private static boolean finished;
 
    public static final float maxSpeed = 40;
    public static final float minSpeed = 10;

    private static float eyeX   = 0, 
                         eyeY   = 0, 
                         eyeZ   = -20, 
                         theta  = 0,
                         phi    = 0,
                         radius = 200,
                         speed  = maxSpeed; 

    private static int w = 1024, h = 600;
    private static int lastMouseX = 0, lastMouseY = 0;

    private static Point cellCursor = new Point();

    // shaders get stored in here... somehow.
    private static int program = 0;

    private static GameManager gm;
 
    /**
     * Application init
     * @param args Commandline args
     */
    public static void main(String[] args) 
    {
        try 
        {
            if(args.length > 0)
            {
                if(args[0].equals("-")) // allow specifying a file on the command line. If that file is '-', then allow reading from stdin
                    gm = new GameManager(LifeGame.fromFile(System.in));
				else if(args[0].equals("-random"))
				{
					if(args.length == 3)
					{
						int n = Integer.parseInt(args[1]);
						int range = Integer.parseInt(args[2]);

						gm = new GameManager(LifeGame.fromRandom(n, range));
					}
					else
					{
						JOptionPane.showMessageDialog(null, "Invalid initial configuration options. Exiting...", "Fatal Error", JOptionPane.ERROR_MESSAGE);
						System.exit(1);
					}
				}
                else if(args[0].equals("-rect-prism"))
                {
                    if(args.length == 4)
                    {
                        int x = Integer.parseInt(args[1]);
                        int y = Integer.parseInt(args[2]);
                        int z = Integer.parseInt(args[3]);

                        gm = new GameManager(LifeGame.fromPrism(x, y, z));
                    }
                    else
                    {
                        JOptionPane.showMessageDialog(null, 
                                                      "Invalid prism configuration options. Exiting...", 
                                                      "Fatal Error", 
                                                      JOptionPane.ERROR_MESSAGE);
                    }
                }
                else
                {
                    try
                    {
                        gm = new GameManager(LifeGame.fromFile(new FileInputStream(args[0])));
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
            else // if not file is specified on the command line, then prompt the user for a file.
            {
                JFileChooser fc = new JFileChooser();
                int retv = fc.showOpenDialog(null);

                if(retv == JFileChooser.APPROVE_OPTION)
                {
                    try
                    {
                        FileInputStream fis = new FileInputStream(fc.getSelectedFile());
                        gm = new GameManager(LifeGame.fromFile(fis));
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
                else
                {
                    JOptionPane.showMessageDialog(null, 
                                                  "No pattern specified. Exiting...", 
                                                  "Fatal Error", 
                                                  JOptionPane.ERROR_MESSAGE);
                    System.exit(1);
                }
            }

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
            System.out.println("Error setting up display");
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
    private static void logic() 
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

            if (!state) // key was released
			{
                switch (key)
                {
                    case Keyboard.KEY_F:
						JFileChooser fc = new JFileChooser();
						int retv = fc.showOpenDialog(null);

						if(retv == JFileChooser.APPROVE_OPTION)
						{
							try
							{
								FileInputStream fis = new FileInputStream(fc.getSelectedFile());
								gm = new GameManager(LifeGame.fromFile(fis));
								autoUpdateMode = false;
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
					case Keyboard.KEY_1:
						spinMode = !spinMode;
						break;
                    case Keyboard.KEY_B:
                        editMode = !editMode;
                        System.err.printf("Edit mode: %s\n", new Boolean(editMode).toString());
                        break;
					case Keyboard.KEY_SPACE:
						gm.updateGame();
						break;
                    case Keyboard.KEY_LSHIFT:
                        speed = maxSpeed;
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
                        gm.addNewCell(cellCursor);
                        break;
                    case 1: // right !
                        gm.removeCell(cellCursor);
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
			{
				eyeY -= speed;
			}
			if (Keyboard.isKeyDown(Keyboard.KEY_E))
			{
				eyeY += speed;
			}

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

        cellCursor = gm.toGridCoords(eyeX + editArmLength * (float)Math.sin(theta), eyeY + editArmLength * (float)Math.sin(phi), eyeZ + editArmLength * (float)Math.cos(theta));

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
			float structCentreX = (float)origin.x + (float)dim.x / 2f * GameManager.boxLength;
			float structCentreY = (float)origin.y + (float)dim.y / 2f * GameManager.boxLength;
			float structCentreZ = (float)origin.z + (float)dim.z / 2f * GameManager.boxLength;

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
        {
            GameManager.renderBox(cellCursor, 0.5f, 1.0f, 0.5f);
        }

        gm.render();
 
        GL11.glPopMatrix();
    }

/*
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
