import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Scanner;

/**
 * Created by Yahosseini on 15.12.2016.
 */
public class Landscape2D implements Landscape {
    private String id = "Landscape";
    private ArrayList<Integer> structure = new ArrayList<>();
    private int edgeSize;
    private  ArrayList<Integer> previousEndField;

    Landscape2D() {
        makeLandscape(63);
    }

    @Override
    public void makeLandscape(int edgeSize) {
        this.edgeSize = edgeSize;
    }

    @Override
    public ArrayList<Landscape> readInLandscapes(String path) {
        ArrayList<Landscape> landscape2DS = new ArrayList<>();
        try (Scanner in = new Scanner(new FileReader(path))) {
            while (in.hasNextLine()) {
                String line = in.nextLine();
                Landscape2D landscape2D = new Landscape2D();
                String[] lineSplit = line.split(",");
                for (String s : lineSplit) {
                    Integer i = Integer.parseInt(s);
                    landscape2D.structure.add(i);
                }
                landscape2DS.add(landscape2D);
            }
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
        return landscape2DS;
    }

    public Integer getMaximalPoints() {return Collections.max(structure);}
    @Override
    public ArrayList<Integer>  getStructure() {
        return structure;
    }


    public int getValue(int x, int y) {
        return structure.get(y + (x * edgeSize));
    }

    @Override
    public void readInPreviousEndFields(String path, ArrayList<Landscape> landscape2DS){
        ArrayList<ArrayList<Integer>> endFields = new ArrayList<ArrayList<Integer>>();
        try (Scanner in = new Scanner(new FileReader(path))) {
            while (in.hasNextLine()) {
                ArrayList<Integer> endFieldsForOneLandscape = new ArrayList<>();
                String line = in.nextLine();
                String[] lineSplit = line.split(",");
                for (String s : lineSplit) {
                    Integer i = Integer.parseInt(s);
                    endFieldsForOneLandscape.add(i);
                }
                endFields.add(endFieldsForOneLandscape);
            }
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
        for(int i = 0; i<= landscape2DS.size(); i++){
            landscape2DS.get(i).setPreviousEndField(endFields.get(i));
        }
    }
    @Override
    public ArrayList<Integer> getPreviousRoundEndField() {
        return previousEndField;
    }

    @Override
    public void setPreviousEndField(ArrayList<Integer> previousEndField) {
        this.previousEndField = previousEndField;
    }
}


