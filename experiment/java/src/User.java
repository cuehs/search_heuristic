import org.eclipse.jetty.websocket.api.Session;

/**
 * Created by Yahosseini on 19.12.2016.
 */
public class User {
    private int userId;
    private Session session;
    private VisibilityLandscape1D visibilityLandscape1D;
    private VisibilityLandscape2D visibilityLandscape2D;
    private State state;

    public State getState() {
        return state;
    }

    public User(Session session, int id) {
        userId = id;
        this.session = session;
        visibilityLandscape1D = new VisibilityLandscape1D();
        visibilityLandscape2D = new VisibilityLandscape2D();
        state = new State();

    }
    public int getId() {
        return userId;
    }

    public void setId(int id) {
        userId = id;
    }

    public Session getSession() {
        return session;
    }

    public VisibilityLandscape getVisibilityLandscape() {
        if (World.getWorld().getLandscapes(this.state.getLandscapeOrder(this.state.getLevel())) instanceof Landscape1D) {
            return visibilityLandscape1D;
        } else {
            return visibilityLandscape2D;
        }
    }
}
