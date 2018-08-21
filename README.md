# DOM-like API for XML

## Example:
```scala
import com.github.rgafiyatullin.xml.common.QName
import com.github.rgafiyatullin.xml.dom.Node

object jabberClient {
  val ns = "jabber:client"

  val error: QName = QName(ns, "error")
  val presence: QName = QName(ns, "presence")
  val iq: QName = QName(ns, "iq")
  val message: QName = QName(ns, "message")
  
  val body: QName = QName(ns, "body")
}

val message = 
  Node(jabberClient.message)
    .withAttribute("type", "chat")
    .withAttribute("from", "romeo@montague.lit")
    .withAttribute("to", "juliet@capulet.lit")
    .withChildren(Seq(
      Node(jabberClient.body)
        .withChildren(Seq(Node("Helloes!")))   
    ))

message.rendered.toString 
// <message type='chat' from='romeo@montague.lit' to='juliet@capulet.lit' xmlns='jabber:client'> 
//   <body xmlns='jabber:client'> 
//     <![CDATA[Helloes!]]>
//   </body>
// </message>
```





