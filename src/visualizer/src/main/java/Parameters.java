
import java.io.File;
import java.io.IOException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.JFileChooser;
import javax.swing.JTextArea;
import java.awt.Color;
import java.awt.Font;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import javax.swing.Timer;

public class Parameters
{
	public static int width = 1200, height = 800;
	public static double minBoxWidth = 20, minBoxHeight = 20;
	public static double zoomFactor = 3.0/4.0, boxFactor = 3.0/4.0;
	public static int boxLines = 3;
	public static StacFrame stFrame;
	public static JTextArea rightArea;
	public static CodeArea leftArea;
    public static SearchArea searchArea;
	public static String pwd = "./";
	public static Color colorFocus = new Color(Integer.parseInt("FFF7BC", 16)),
			colorSelection = new Color(Integer.parseInt("A6BDDB", 16)),
			colorHighlight = new Color(Integer.parseInt("2B8CBE", 16));
	public static int transparency = 160;
	public static Font font = new Font("Serif", Font.PLAIN, 14);
	
	public static boolean debug = false;
	public static long interval = 5000, startTime, lastInterval, refreshInterval = 200;
	public static long mouseInterval = 100, mouseLastTime;
	public static boolean highlightIncoming = false, highlightOutgoing = false, vertexHighlight = true;
    
    public static boolean pingStart=false, pingEnd=false, pingRespondedMain = false, pingRespondedContext = false;
    public static Timer pinger;
    public static boolean fixCaret = false;
	
	public static int debug1, debug2, val;
	
	public static long limitV = Long.MAX_VALUE;
	public static boolean cut_off = true;
	
	public static void setRightText()
	{
		StringBuilder text = new StringBuilder();
		for(Vertex v : Main.graph.vertices)
		{
			//if(v.isHighlighted)
            if(v.isSelected)
				text.append(v.getRightPanelContent() + "\n\n");
		}
		
		for(MethodVertex v : Main.graph.methodVertices)
		{
            //if(v.isHighlighted)
            if(v.isSelected)
				text.append(v.getRightPanelContent() + "\n\n");
		}
		
		for(MethodPathVertex v : Main.graph.methodPathVertices)
		{
            //if(v.isHighlighted)
            if(v.isSelected)
				text.append(v.getRightPanelContent() + "\n\n");
		}
		
		rightArea.setText(text.toString());
		rightArea.setCaretPosition(0);
	}

	public static File openFile(boolean includeDirectories)
	{
		try
		{
			JFileChooser choose = new javax.swing.JFileChooser(new File(pwd).getCanonicalPath());
			if(includeDirectories)
				choose.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);

			int ret = choose.showOpenDialog(null);
			if(ret == JFileChooser.APPROVE_OPTION)
			{
				pwd = Parameters.folderFromPath(choose.getSelectedFile().getAbsolutePath());
				return choose.getSelectedFile();
			}
			else return null;
		}
		catch(IOException ex)
		{
			return null;
		}
	}
	
	public static String dropExtension (String path)
	{
		String extLessPath;
		
		int lastPoint = path.lastIndexOf(".");
		if(lastPoint==-1)
			return path;
		
		extLessPath = path.substring(0, lastPoint);
		return extLessPath;
	}

	public static String fileNameFromPath (String path)
	{
		int lastSlash = Math.max(path.lastIndexOf("/"), path.lastIndexOf("\\"));
		String fileName = path.substring(lastSlash+1);
				
		return Parameters.dropExtension(fileName);
	
	}


	public static String folderFromPath (String path)
	{
//		System.out.println("path is: "+path);
		int lastSlash = Math.max(path.lastIndexOf("/"), path.lastIndexOf("\\"));
		
		if(lastSlash==-1)
			return "/";

		
		String folder = path.substring(0,lastSlash);
				
		return folder;
	
	}
	
	
	public static String extensionFromPath(String path)
	{
		int lastSlash = path.lastIndexOf(".");
		return path.substring(lastSlash+1);
				
	}
    
    public static String getHTMLVerbatim(String str)
    {
        int pos = -1;
        String suf = ""+str;

        String pre = "";
        while(true)
        {
            pos = suf.indexOf('&');
            if(pos<0)
                break;
            pre = pre + suf.substring(0,pos)+"&amp;";
            suf = suf.substring(pos+1);
        }
        suf = pre + suf;
        
        pre = "";
        while(true)
        {
            pos = suf.indexOf('<');
            if(pos<0)
                break;
            pre = pre + suf.substring(0,pos)+"&lt;";
            suf = suf.substring(pos+1);
        }
        suf = pre + suf;
 
        pre = "";
        while(true)
        {
            pos = suf.indexOf('>');
            if(pos<0)
                break;
            pre = pre + suf.substring(0,pos)+"&gt;";
            suf = suf.substring(pos+1);
        }
        suf = pre + suf;
        
        return "<code>"+suf+"</code>";
    }

	public static void repaintAll()
	{
		leftArea.setDescription();
		setRightText();
        searchArea.writeText();
		stFrame.repaint();
        
        if(Parameters.fixCaret)
        {
            Parameters.fixCaret = false;
            Parameters.fixCaretPositions();
        }
	}
	
    
    public static void fixCaretPositions()
    {
        leftArea.fixCaretPosition();
        searchArea.fixCaretPosition();
    }
    
    
    public static void ping()
    {
        Parameters.pingStart = true;
        Parameters.pingEnd = false;
        Parameters.fixCaret = true;

        int delay = 1000;
        ActionListener pingListener = new ActionListener()
        {
            public void actionPerformed(ActionEvent e)
            {
                if(Parameters.pingEnd)
                {
                    Parameters.pinger.stop();
                    Parameters.pingEnd = false;
                }
                else
                {
                    Parameters.pingEnd = true;
                }
                Parameters.repaintAll();
            }
        };
        
        Parameters.pinger = new Timer(delay, pingListener);
        Parameters.pinger.setRepeats(true);
        Parameters.pinger.start();
    }
    
    
	public static void test()
	{
//		String desc ="\"sootStmt\":\"b0 = 3\",\n        \"sootMethod\":{          \"$type\":\"soot.SootMethod\",          \"declaringClass\":\"Loop\",          \"name\":\"main\",          \"parameterTypes\":[            \"java.lang.String[]\"          ],          \"returnType\":\"void\",          \"modifiers\":9,          \"exceptions\":[                      ]        },        \"index\":1,        \"line\":3,        \"column\":-1,        \"sourceFile\":\"Loop.java\" ";
		String desc = "\"sootStmt\":\"r0 := @this: java.util.HashMap\",\n        \"sootMethod\":{\n          \"$type\":\"soot.SootMethod\",\n          \"declaringClass\":\"java.util.HashMap\",\n          \"name\":\"<init>\",\n          \"parameterTypes\":[\n            \"int\",\n            \"float\"\n          ],\n          \"returnType\":\"void\",\n          \"modifiers\":1,\n          \"exceptions\":[\n           \n          ]\n        },\n        \"index\":0,\n        \"line\":-1,\n        \"column\":-1,\n        \"sourceFile\":\"HashMap.java\"";

		System.out.println(desc);
		Pattern stmtPattern = Pattern.compile(
				"\"sootStmt\":\"(.*)\",\\n\\s*"
				+ "\"sootMethod\":\\{\\s*"
				+ "\"\\$type\":\"soot.SootMethod\",\\s*"
				+ "\"declaringClass\":\"(.*)\",\\s*"
				+ "\"name\":\"(.*)\",\\s*"
				+ "\"parameterTypes\":\\[\\s*([[^,]*,\\s*)]+.*)\\s*\\],\\s*"
				+ "\"returnType\":\"(.*)\",\\s*"
				+ "\"modifiers\":(\\d+),\\s*"
				+ "\"exceptions\":\\[\\s*(.*)\\s*\\]\\s*\\},\\s*"
				+ "\"index\":(-?\\d+),\\s*"
				+ "\"line\":(-?\\d+),\\s*"
				+ "\"column\":(-?\\d+),\\s*"
				+ "\"sourceFile\":\"(.*)\"\\s*"
				);
		
		Matcher stmtMatcher = stmtPattern.matcher(desc);
		if(stmtMatcher.find())
			System.out.println(stmtMatcher.group(0));
		else
			System.out.println("not found");
		
//		String description = this.stmtMatcherToDesc(stmtMatcher);
//		String inst = this.stmtMatcherToInst(stmtMatcher);
//		String method1 = stmtMatcherToMethod(stmtMatcher);
//		int index1 = stmtMatcherToIndex(stmtMatcher);
		
//		System.out.println(description);

//		JOptionPane.showMessageDialog(null, desc);

	}
	
}
