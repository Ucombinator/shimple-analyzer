
//TODO: Save list of matching vertices here to speed up highlighting?
public class Instruction
{
	public String str;
	public String methodName;
	public boolean isInstr;
	public int jimpleIndex;
	public boolean isCycleHighlighted;
	public boolean isHighlighted;
    public boolean isSelected;

	public int startIndex;
	public int endIndex;
	
	public Instruction(String methodName)
	{
		this.str = "";
		this.methodName = methodName;
		this.isInstr = false;
		this.jimpleIndex = -1;
		this.isHighlighted = false;
        this.isCycleHighlighted = false;
        this.isSelected = false;
		this.startIndex = -1;
		this.endIndex = -1;
	}
	
	public Instruction(String str, String method, boolean isInstr, int jimpleIndex)
	{
		this.str = str;
		this.methodName = method;
		this.isInstr = isInstr;
		this.jimpleIndex = jimpleIndex;
		this.isHighlighted = false;
        this.isCycleHighlighted = false;
        this.isSelected = false;
		this.startIndex = -1;
		this.endIndex = -1;
	}
}
