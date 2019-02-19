import java.io.BufferedWriter;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;

/**
 * Created by Yahosseini on 27.01.2017.
 */
public class Logger {
    private static BufferedWriter movementWriter;
    private static BufferedWriter structureWriter;
    private static BufferedWriter participantWriter;
    Logger(){

        restartFiles(World.getWorld().getPathPrefix());

    }
    public void writeToStructureFile(String string) {
        try {
            structureWriter.write(string);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
    public void flushStructureFile(){
        try {
            structureWriter.flush();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
    public void writeToMovementFile(String string) {
        try {
            movementWriter.write(string);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
    public void flushMovementFile(){
        try {
            movementWriter.flush();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public void writeToParticipantFile(String string) {
        try {
            participantWriter.write(string);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
    public void flushParticipantFile(){
        try {
            participantWriter.flush();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
    public void restartFiles(String path){

        String participantFilename = path+"participant.csv";
        String movementFilename = path+"movement.csv";
        String structureFilename = path+"structure.csv";
        try {
            participantWriter = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(participantFilename), "utf-8"));
            movementWriter = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(movementFilename), "utf-8"));
            structureWriter= new BufferedWriter(new OutputStreamWriter(new FileOutputStream(structureFilename), "utf-8"));
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
