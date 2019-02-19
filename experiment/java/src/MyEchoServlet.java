import org.eclipse.jetty.websocket.servlet.WebSocketServlet;
import org.eclipse.jetty.websocket.servlet.WebSocketServletFactory;

import javax.servlet.annotation.WebServlet;

//TODO

@SuppressWarnings("serial")
@WebServlet(name = "MyEcho WebSocket Servlet", urlPatterns = {"/echo"})
public class MyEchoServlet extends WebSocketServlet {
    @Override
    public void configure(WebSocketServletFactory factory) {
        // set a 30 second timeout
        factory.getPolicy().setIdleTimeout(600000);

        // register MyEchoSocket as the WebSocket to create on Upgrade
        factory.register(AdapterWorldSocket.class);
    }
}