
import java.util.ArrayList;

public class Method
{
	public Class ourClass;
	private final String methodName;
	//private final String functionName;
	ArrayList<Vertex> vertices;
	ArrayList<Instruction> instructionList;
	
	public Method(String methodName)
	{
		//System.out.println("Creating method: " + methodName);
		this.methodName = methodName;

		if(!methodName.equals("ErrorState"))
		{
			String[] splitMethodName = methodName.split(" ");

			//Remove beginning angle bracket and ending colon
			String className = splitMethodName[0].substring(1, splitMethodName[0].length() - 1);
			this.addClass(className);

			//this.functionName = splitMethodName[1];
		}


		vertices = new ArrayList<Vertex>();
		instructionList = new ArrayList<Instruction>();
	}
	
	public String getFullName()
	{
		return this.methodName;
	}

	public String getClassName()
	{
		return this.ourClass.getClassName();
	}

	/*public String getFunctionName()
	{
		return this.functionName;
	}*/

	public void addClass(String className)
	{
		this.ourClass = Main.graph.classes.get(className);
		if(this.ourClass == null)
		{
			this.ourClass = new Class(className);
			Main.graph.classes.put(className, this.ourClass);
			System.out.println("Adding new class: " + className);
		}

		this.ourClass.addMethod(this);
	}
	
	public void addVertex(Vertex v)
	{
		this.vertices.add(v);
	}
	
	public ArrayList<Instruction> getInstructionList()
	{
		return this.instructionList;
	}

	//Since we override the equals method, we must also override the hashCode method.
	public int hashCode()
	{
		return this.methodName.hashCode();
	}
	
	public boolean equals(Object other)
	{
		if(other instanceof Method)
			return this.methodName.equals(((Method) other).methodName);
		else
			return false;
	}
	
	public void print()
	{
		System.out.println(this.methodName);
	}
	
	//This has to be here instead of in the MethodVertex class, because a method vertex
	//might only have part of the code for the method.
	public void collectAndSortInstructions()
	{
		this.instructionList = new ArrayList<Instruction>();
	
		for(Vertex v : this.vertices)
		{
			if(v.jimpleIndex >= 0)
			{
				while(instructionList.size() <= v.jimpleIndex)
					instructionList.add(new Instruction(this.methodName));
				
				String newInst = v.jimpleIndex + ":  " + v.getInstruction() + "\n";
				this.instructionList.set(v.jimpleIndex, new Instruction(newInst, this.methodName, true, v.jimpleIndex));
			}
		}
		
		//Remove empty instructions
		/*for(int i = 0; i < instructionList.size(); i++)
		{
			if(instructionList.get(i).str == "")
			{
				this.instructionList.remove(i);
				i--;
			}
		}*/
	}
	
	public void highlightInstructions()
	{
		//System.out.println("Recalculating highlights for instructions in method " + this.methodName);
		for(Instruction inst : this.instructionList)
		{
			inst.isHighlighted = false;
			inst.isCycleHighlighted = false;
            inst.isSelected = false;
		}
		
		for(Vertex v : this.vertices)
		{
			if(v.jimpleIndex >= 0)
			{
				if(v.isHighlighted())
				{
					this.instructionList.get(v.jimpleIndex).isHighlighted = true;
					//System.out.println("Highlighting " + v.jimpleIndex + ", " + v.getMethodName());
				}
				
				if(v.isCycleHighlighted())
				{
					this.instructionList.get(v.jimpleIndex).isCycleHighlighted = true;
					//System.out.println("Cycle highlighting " + v.jimpleIndex + ", " + v.getMethodName());
				}

                if(v.isSelected())
                {
                    this.instructionList.get(v.jimpleIndex).isSelected = true;
                }
			}
		}
	}
}
