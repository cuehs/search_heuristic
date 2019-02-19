import java.util.ArrayList;

/**
 * Created by Yahosseini on 15.12.2016.
 */
public class VisibilityLandscape2D implements VisibilityLandscape {
    private String id = "VisibilityMap";
    private ArrayList<Integer> structure = new ArrayList<>();
    private int edgeSize;
    private Integer position;

    VisibilityLandscape2D() {
        makeVisibilityLandscape(63);
    }

    public void makeVisibilityLandscape(int edgeSize) {
        this.edgeSize = edgeSize;
        for (int i = 0; i < edgeSize; i++) {
            for (int j = 0; j < edgeSize; j++) {
                structure.add(0);
            }
        }
        startPosition();
    }
/*
    public int getEdgeSize() {
        return edgeSize;
    }

    public VisibilityLandscape2D getVisibilityLandscape() {
        return this;
    }

    public ArrayList<Integer> getStructure() {
        return structure;
    }

    public int isVisible(int x, int y) {
        return structure.get(y + (x * edgeSize));
    }
*/
    public void resetVisibilityLandscapeAndPosition() {
        for (int i = 0; i < structure.size(); i++) {
            structure.set(i, 0);
        }
        this.startPosition();

    }

    public void resetVisibilityLandscape() {
        for (int i = 0; i < structure.size(); i++) {
            structure.set(i, 0);
        }

    }

    public int getPosition() {
        return position;
    }

    public void setPosition(int position) {
        this.position = position;

    }

    @Override
    public void startPosition() {
        this.position = 1984;
    }

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
        //left border
        if ((position % edgeSize) == 0) {
            neighbours.add(position + 1 - edgeSize);
            neighbours.add(position + 1);
            neighbours.add(position + 1 + edgeSize);
        }
        //right border
        else if ((position % edgeSize) == (edgeSize-1)) {
            neighbours.add(position - 1 - edgeSize);
            neighbours.add(position - 1);
            neighbours.add(position - 1 + edgeSize);
        }
        //not right or left border
        else {
            neighbours.add(position + 1 - edgeSize);
            neighbours.add(position + 1);
            neighbours.add(position + 1 + edgeSize);
            neighbours.add(position - 1 - edgeSize);
            neighbours.add(position - 1);
            neighbours.add(position - 1 + edgeSize);
        }
        //upper
        neighbours.add(position + edgeSize);
        //lower
        neighbours.add(position - edgeSize);
        for (int i : neighbours) {
            if (i < 0 || i >= structure.size()) {
                invalidNeighbours.add(i);
            }
        }
        neighbours.removeAll(invalidNeighbours);
        return neighbours;

    }

    public void makeVisibleByOther(int position) {
        int value = structure.get(position);
        if (value == 0 || value == 2) {
            structure.set(position, value + 1);
        }
    }

}



