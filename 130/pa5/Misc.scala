import java.io.File

/********************************************************/
/********** Helper Functions: Read/Write Files **********/
/********************************************************/

object Lines {
  
  def iterator(file: String) : Iterator[String] = { 
    val f = new File(file)
    scala.io.Source.fromFile(f).getLines()
  }
 
  def list(file: String) : List[String] = { 
    val buf   = scala.io.Source.fromFile(new File(file)) 
    val lines = buf.getLines().toList
    buf.close()
    lines 
  }

}

// vim: set ts=2 sw=2 et:

