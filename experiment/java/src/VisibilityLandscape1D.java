import java.util.ArrayList;

/**
 * Created by Yahosseini on 15.12.2016.
 */
public class VisibilityLandscape1D implements VisibilityLandscape {
    private String id = "VisibilityMap";
    private ArrayList<Integer> structure = new ArrayList<>();
    private int edgeSize;
    private Integer position;

    VisibilityLandscape1D() {
        makeVisibilityLandscape(63);
    }

    @Override
    public void makeVisibilityLandscape(int edgeSize) {
        this.edgeSize = edgeSize;
        for (int i = 0; i < edgeSize; i++) {
                structure.add(0);
        }
        this.startPosition();

    }


    @Override
    public void resetVisibilityLandscapeAndPosition() {
        for (int i = 0; i < structure.size(); i++) {
            structure.set(i, 0);
        }
        this.startPosition();

    }

    @Override
    public void resetVisibilityLandscape() {
        for (int i = 0; i < structure.size(); i++) {
            structure.set(i, 0);
        }

    }

    @Override
    public int getPosition() {
        return position;
    }

    @Override
    public void setPosition(int position) {
        this.position = position;

    }

    @Override
    public void startPosition() {
        this.position = 0;
    }

    @Override
    public void makeVisibleBySelf(int position) {
        ArrayList<Integer> neighbours = findNeighbours(position);
        neighbours.add(position);
        for (int i : neighbours) {
            if (structure.get(i) == 0 || structure.get(i) == 1) {
                structure.set(i, structure.get(i) + 2);
            }
        }

    }

    private ArrayList<Integer> findNeighbours(int position) {
        ArrayList<Integer> neighbours = new ArrayList<>();
        ArrayList<Integer> invalidNeighbours = new ArrayList<>();
        neighbours.add(position +1);
        neighbours.add(position -1);

        for (int i : neighbours) {
            if (i < 0 || i >= structure.size()) {
                invalidNeighbours.add(i);
            }
        }
        neighbours.removeAll(invalidNeighbours);
        return neighbours;

    }
    
    @Override
    public void makeVisibleByOther(int position) {
        int value = structure.get(position);
        if (value == 0 || value == 2) {
            structure.set(position, value + 1);
        }
    }

}



