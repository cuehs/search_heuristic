import java.util.ArrayList;

public interface Landscape {
    void makeLandscape(int edgeSize);

    ArrayList<Landscape> readInLandscapes(String path);

    ArrayList<Integer>  getStructure();

    void readInPreviousEndFields(String path, ArrayList<Landscape> landscape);

    ArrayList<Integer> getPreviousRoundEndField();

    void setPreviousEndField(ArrayList<Integer> previousEndField);
}
