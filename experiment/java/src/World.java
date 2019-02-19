import org.eclipse.jetty.websocket.api.Session;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArraySet;

/**
 * Created by Yahosseini on 15.12.2016.
 */
public final class World {
    public int NUMBEROFLEVEL;
    private String pathPrefix="./individualnew/";
    private static World instance = null;
    private ArrayList<Landscape> landscapes = new ArrayList<>();
    private Set<User> users = new CopyOnWriteArraySet<>();
    private static Logger logger;
    private int userIdNumber = 0;
    private int landscapesPerCategory;

    private World() {
        //*3 afterwards to factor all conditions
        NUMBEROFLEVEL = 20;
        Landscape2D landscape2D = new Landscape2D();
        Landscape1D landscape1D = new Landscape1D();
        //all three files must contain the same number of landscapes! (lines)
        landscapes.addAll(landscape2D.readInLandscapes(pathPrefix+"landscape2Dsearch.csv"));
        landscapes.addAll(landscape1D.readInLandscapes(pathPrefix+"landscape1Dstop.csv"));
        landscapes.addAll(landscape2D.readInLandscapes(pathPrefix+"landscape2Dboth.csv"));
        landscapesPerCategory = (landscapes.size() / 3);

    }

    public static World initialize() {
        if (instance == null) {

            instance = new World();
            logger = new Logger();
        }
        return instance;
    }

    public static World getWorld() {
        return instance;
    }

    public String getPathPrefix() {
        return pathPrefix;
    }

    public Landscape getLandscapes(int index) {
        return landscapes.get(index);
    }

    public int getNumberOfLandscapes() {
        return landscapes.size();
    }

    public void addUser(Session session) {
        User user = new User(session, userIdNumber);
        users.add(user);
        userIdNumber++;
        int i = 0;
        for (LevelType lt : user.getState().getLevelTypes()) {
            logger.writeToStructureFile(user.getId() + "," + i + "," + lt.toCsvString() + "\n");
            i++;
        }
        logger.flushStructureFile();

    }


    public void removeUser(Session session) {
        for (User user : users) {
            if (user.getSession().equals(session)) {
                users.remove(user);
                System.out.println("Removed user: " + user.getId() );
            }
        }

    }



    public User getUser(Session session) {
        for (User user : users) {
            if (user.getSession().equals(session)) {
                return user;
            }
        }
        return null;
    }



    public Set<User> getAllUsers() {
        return users;
    }



    public void updateVisibilityLandscapes(Session session) {
        User currentUser = getUser(session);
        if (currentUser.getState().getIsCooperative(currentUser.getState().getLevel())) {
                currentUser.getVisibilityLandscape().resetVisibilityLandscape();
                int field = currentUser.getVisibilityLandscape().getPosition();
                currentUser.getVisibilityLandscape().makeVisibleBySelf(field);
                currentUser.getVisibilityLandscape().makeVisibleByOther(1);
        } else {
            currentUser.getVisibilityLandscape().resetVisibilityLandscape();
            int field = currentUser.getVisibilityLandscape().getPosition();
            currentUser.getVisibilityLandscape().makeVisibleBySelf(field);

        }
    }

    public int getLandscapeIds(int number, int stage) {
        int modulated = userIdNumber % 100;
        return(((modulated*NUMBEROFLEVEL) + (landscapesPerCategory * stage))+ number);
    }

    public static Logger getLogger() {
        return logger;
    }

    public String getTime() {
        return Instant.now().toString();
    }

}
