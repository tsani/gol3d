import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import java.util.*;
import java.io.*;



@SuppressWarnings("serial")
public class GridDesign extends JFrame {
   // Named-constants for the game board
   public static final int ROWS = 120;  // ROWS by COLS cells
   public static final int COLS = 120;
   public int[] heightY = {1};
   public static final double  timeOfDay= System.currentTimeMillis();
   public final File file =  new File( System.getProperty("user.dir") + "\\directory\\" + timeOfDay +".txt"); //sketchy as fuck

   public final ArrayList<Integer> integerlist = new ArrayList<Integer>();
   public static final Scanner scan = new Scanner(System.in);

   public static final int CELL_SIZE = 10; // cell width and height (square)
   public static final int CANVAS_WIDTH = CELL_SIZE * COLS;  // the drawing canvas
   public static final int CANVAS_HEIGHT = CELL_SIZE * ROWS;
   public static final int GRID_WIDTH = 1;                   // Grid-line's width
   public static final int GRID_WIDHT_HALF = GRID_WIDTH / 2; // Grid-line's half-width
   // Symbols (cross/nought) are displayed inside a cell, with padding from border

   public static final int CELL_PADDING = CELL_SIZE / 6;
   public static final int SYMBOL_STROKE_WIDTH = 2; // pen's stroke width


   // Use an enumeration (inner class) to represent the various states of the game
   public enum GameState {
      PLAYING, DRAW
   }
   private GameState currentState;  // the current game state

   // Use an enumeration (inner class) to represent the seeds and cell contents
   public enum Seed {
      EMPTY, CROSS
   }
   private Seed currentPlayer;  // the current player

   private Seed[][] board   ; // Game board of ROWS-by-COLS cells
   private DrawCanvas canvas; // Drawing canvas (JPanel) for the game board
   private JLabel statusBar;  // Status Bar


   /** Constructor to setup the game and the GUI components */
   public GridDesign() throws Exception{


		file.createNewFile();



      canvas = new DrawCanvas();  // Construct a drawing canvas (a JPanel)
      canvas.setPreferredSize(new Dimension(CANVAS_WIDTH, CANVAS_HEIGHT));


	     canvas.addKeyListener(new KeyAdapter() {
		  //@Override
		  public void keyboardPress(KeyEvent ev) {

			  if(ev.getKeyCode() == KeyEvent.VK_1)
			  {
				try {
				  for (int row = 0; row < ROWS; ++row) {
				        for (int col = 0; col < COLS; ++col) {
				          board[row][col] = Seed.EMPTY; // all cells empty
				        }
     				    }

				  heightY[0] = 2;

			       	} catch(Exception e)
			       	   {
						}
				repaint();
			   }

		 	}
			});




      canvas.addMouseListener(new MouseAdapter() {
         @Override
         public void mouseClicked(MouseEvent e)  {  // mouse-clicked handler
            int mouseX = e.getX();
            int mouseY = e.getY();


            // Get the row and column clicked
            int rowSelected = mouseY / CELL_SIZE;
            int colSelected = mouseX / CELL_SIZE;





            if (currentState == GameState.PLAYING) {


		  if (rowSelected <= 1 && rowSelected< ROWS && colSelected <= 1
				 && colSelected < COLS )
			 {
				 try {
					  for (int row = 0; row < ROWS; ++row) {
				     	 for (int col = 0; col < COLS; ++col) {
						 board[row][col] = Seed.EMPTY; // all cells empty
					     }
					    }

				  heightY[0] += 1;

						} catch(Exception errs)
					 {
					}

			}


               if (rowSelected > 1 && rowSelected < ROWS && colSelected > 1
                     && colSelected < COLS && board[rowSelected][colSelected] == Seed.EMPTY) {
                 board[rowSelected][colSelected] = currentPlayer; // Make a move




				try{
					 integerlist.add(rowSelected);
					 integerlist.add(colSelected);
					 for(int i = 0; i + 1< integerlist.size(); i++)
					 {


						PrintWriter printers = new PrintWriter((new BufferedWriter(new FileWriter(file, true))));
						int w = integerlist.get(i);
						int l = integerlist.get(i + 1);
						String w1 = String.valueOf(w);
						String l1 = String.valueOf(l);

						String height = String.valueOf(heightY[0]);


						String conc = w1 + " " + l1 + " " + height;
						printers.println(conc);
						System.out.println(conc);
						//System.out.println(height);

						printers.flush();


					 }

				                } catch(Exception errs)
				                 {
									 System.err.print(errs);
				 					}
               }
            }
            // Refresh the drawing canvas
            repaint();  // Call-back paintComponent().
         }
      });

      // Setup the status bar (JLabel) to display status message
      statusBar = new JLabel("  ");
      statusBar.setFont(new Font(Font.DIALOG_INPUT, Font.BOLD, 15));
      statusBar.setBorder(BorderFactory.createEmptyBorder(2, 5, 4, 5));

      Container cp = getContentPane();
      cp.setLayout(new BorderLayout());
      cp.add(canvas, BorderLayout.CENTER);
      cp.add(statusBar, BorderLayout.PAGE_END); // same as SOUTH

      setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
      pack();  // pack all the components in this JFrame
      setTitle("patternCreator");
      setVisible(true);  // show this JFrame

      board = new Seed[ROWS][COLS]; // allocate array
      initGame(); // initialize the game board contents and game variables
   }

   /** Initialize the game-board contents and the status */
   public void initGame() throws Exception{
      for (int row = 0; row < ROWS; ++row) {
         for (int col = 0; col < COLS; ++col) {
            board[row][col] = Seed.EMPTY; // all cells empty
         }
      }
      currentState = GameState.PLAYING; // ready to play
      currentPlayer = Seed.CROSS;       // cross plays first
   }





   /**
    *  Inner class DrawCanvas (extends JPanel) used for custom graphics drawing.
    */
   class DrawCanvas extends JPanel {
      @Override
      public void paintComponent(Graphics g) {  // invoke via repaint()
         super.paintComponent(g);    // fill background
         setBackground(Color.WHITE); // set its background color

         // Draw the grid-lines
         g.setColor(Color.LIGHT_GRAY);
         for (int row = 1; row < ROWS; ++row) {
            g.fillRoundRect(0, CELL_SIZE * row - GRID_WIDHT_HALF,
                  CANVAS_WIDTH-1, GRID_WIDTH, GRID_WIDTH, GRID_WIDTH);
         }
         for (int col = 1; col < COLS; ++col) {
            g.fillRoundRect(CELL_SIZE * col - GRID_WIDHT_HALF, 0,
                  GRID_WIDTH, CANVAS_HEIGHT-1, GRID_WIDTH, GRID_WIDTH);
         }

         // Draw the Seeds of all the cells if they are not empty
         // Use Graphics2D which allows us to set the pen's stroke

         Graphics2D g2d = (Graphics2D)g;
         g2d.setStroke(new BasicStroke(SYMBOL_STROKE_WIDTH, BasicStroke.CAP_ROUND,
               BasicStroke.JOIN_ROUND));  // Graphics2D only
         for (int row = 0; row < ROWS; ++row) {
            for (int col = 0; col < COLS; ++col) {
               int x1 = col * CELL_SIZE + CELL_PADDING;
               int y1 = row * CELL_SIZE + CELL_PADDING;
               if (board[row][col] == Seed.CROSS) {
                  g2d.setColor(Color.BLACK);
                  int x2 = (col + 1) * CELL_SIZE - CELL_PADDING;
                  int y2 = (row + 1) * CELL_SIZE - CELL_PADDING;
                  g2d.drawLine(x1, y1, x2, y2);

               }
            }
         }


         // Print status-bar message
         if (currentState == GameState.PLAYING) {
            statusBar.setForeground(Color.BLACK);

      }
  	}
   }











   /** The entry main() method */
   public static void main(String[] args) throws Exception{
      // Run GUI codes in the Event-Dispatching thread for thread safety
       GridDesign p = new GridDesign(); // Let the constructor do the job

      SwingUtilities.invokeLater(new Runnable() {

		  @Override
         public void run() {
			 try{
            	GridDesign p;  // Let the constructor do the job

				}catch(Exception e)
			{
			}

         }



      });

      //p.closes();

	}
}