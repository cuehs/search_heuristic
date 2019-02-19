import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Scanner;

/**
 * Created by Yahosseini on 15.12.2016.
 */
public class Landscape1D implements Landscape {
    private String id = "Landscape";
    private ArrayList<Integer> structure = new ArrayList<>();
    private int edgeSize;
    private  ArrayList<Integer> previousEndField;

    Landscape1D() {
        makeLandscape(63);
    }

    public void makeLandscape(int edgeSize) {
        this.edgeSize = edgeSize;
    }

    public ArrayList<Landscape> readInLandscapes(String path) {
        ArrayList<Landscape> landscapes = new ArrayList<>();
        try (Scanner in = new Scanner(new FileReader(path))) {
            while (in.hasNextLine()) {
                String line = in.nextLine();
                Landscape1D landscape = new Landscape1D();
                String[] lineSplit = line.split(",");
                for (String s : lineSplit) {
                    Integer i = Integer.parseInt(s);
                    landscape.structure.add(i);
                }
                landscapes.add(landscape);
            }
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
        return landscapes;
    }
    public Integer getMaximalPoints() {return Collections.max(structure);}
    public ArrayList<Integer>  getStructure() {
        return structure;
    }


    public int getValue(int x, int y) {
        return structure.get(y + (x * edgeSize));
    }

    public void readInPreviousEndFields(String path, ArrayList<Landscape> landscapes){
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
        for(int i =0 ; i<=landscapes.size();i++){
            landscapes.get(i).setPreviousEndField(endFields.get(i));
        }
    }
    public ArrayList<Integer> getPreviousRoundEndField() {
        return previousEndField;
    }

    public void setPreviousEndField(ArrayList<Integer> previousEndField) {
        this.previousEndField = previousEndField;
    }
}


