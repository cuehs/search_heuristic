import com.google.gson.Gson;
import org.eclipse.jetty.websocket.api.RemoteEndpoint;
import org.eclipse.jetty.websocket.api.Session;
import org.eclipse.jetty.websocket.api.WebSocketAdapter;

public class AdapterWorldSocket extends WebSocketAdapter {
    private World world = World.initialize();
    private Gson gson = new Gson();
    @Override
    public void onWebSocketConnect(Session session) {
        super.onWebSocketConnect(session);
        world.addUser(session);

        if (isConnected()) {
            User currentUser = world.getUser(getSession());
            System.out.println("Added user: " + currentUser.getId());

            currentUser.getSession().getRemote().
                    sendStringByFuture(gson.toJson(currentUser.getState()));
        }

    }

    @Override
    public void onWebSocketText(String message) {

        if (isConnected()) {
            User currentUser = world.getUser(getSession());
            RemoteEndpoint currentEndpoint = currentUser.getSession().getRemote();
            if (message.startsWith("position")) {

                //msg processing
                String[] splitted = message.split(":");
                Integer field = Integer.parseInt(splitted[1]);
                currentUser.getVisibilityLandscape().setPosition(field);
                writeMovementString(getSession(), field, "position");
                world.updateVisibilityLandscapes(getSession());
                currentEndpoint.sendStringByFuture(gson.toJson(currentUser.getVisibilityLandscape()));
                currentUser.getState().nextStep();
                currentUser.getState().fixState();
                currentEndpoint.sendStringByFuture(gson.toJson(currentUser.getState()));
    }

            if (message.startsWith("nextMap")) {
                PingContainer pingNew = new PingContainer();
                pingNew.setId("New");
                currentUser.getVisibilityLandscape().resetVisibilityLandscapeAndPosition();
                world.getLogger().flushMovementFile();
                world.updateVisibilityLandscapes(getSession());

                writeMovementString(currentUser.getSession(), currentUser.getVisibilityLandscape().getPosition(), "start");
                currentEndpoint.sendStringByFuture(gson.toJson(pingNew));

                currentEndpoint.sendStringByFuture(gson.toJson(currentUser.getState()));
                currentEndpoint.sendStringByFuture(gson.toJson(
                        world.getLandscapes(currentUser.getState().getLandscapeOrder(currentUser.getState().getLevel()))));
                currentEndpoint.sendStringByFuture(gson.toJson(currentUser.getVisibilityLandscape()));
            }

            if (message.startsWith("participant")) {
                String[] splitted = message.split(":");
                writeParticipantString(getSession(), splitted[1]);
                world.getLogger().flushParticipantFile();
            }
            if (message.startsWith("remove")) {
                for (User u : world.getAllUsers()) {
                    u.getSession().close();
                    world.removeUser(u.getSession());
                }
                world.getLogger().restartFiles(world.getPathPrefix());
            }

        }
    }

    private void writeMovementString(Session session, int field, String type) {
        User currentUser = world.getUser(session);
        int stage = currentUser.getState().getStage();
        int level = currentUser.getState().getLevel();
        int step = currentUser.getState().getStep();
        if(!type.equals("start") ){step++;}
        world.getLogger().writeToMovementFile(world.getTime() + "," + stage + "," +
                level + "," + step + "," + currentUser.getId() + "," + type + "," + field + "," +
                world.getLandscapes(currentUser.getState().getLandscapeOrder(level)).getStructure().get(currentUser.getVisibilityLandscape().getPosition()) +
                "\n");
    }

    private void writeParticipantString(Session session, String text) {
        User currentUser = world.getUser(session);

        int level = currentUser.getState().getLevel();

        world.getLogger().writeToParticipantFile(world.getTime() + ","
                + currentUser.getId() + "," + text + "\n");

    }

    @Override
    public void onWebSocketClose(int statusCode, String reason) {

        world.removeUser(getSession());

    }
}