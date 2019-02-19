import java.util.ArrayList;
import java.util.Collections;

/**
 * Created by Yahosseini on 06.01.2017.
 */
public class State {

    private int level;
    private int step;
    private int stage;
    private ArrayList<LevelType> levelTypes = new ArrayList<>();
    private String id = "State";

    public State() {
        this.level = 0;
        this.step = 0;
        this.stage = 0;
        ArrayList<LevelType> tmpLevelTypes = new ArrayList<>();
        World world = World.getWorld();
        for (int i = 0; i < 3; i++) {
            tmpLevelTypes.clear();
            for (int j = 0; j < (World.getWorld().NUMBEROFLEVEL); j++) {
                LevelType levelType = new LevelType();
                levelType.setStepsPlayed(30);
                levelType.setCooperative(false);
                levelType.setLandscapeOrder(world.getLandscapeIds(j,i));
                levelType.setRich((j % 2) < 1);
                levelType.setStage(i);
                tmpLevelTypes.add(levelType);
            }

            Collections.shuffle(tmpLevelTypes);
            levelTypes.addAll(tmpLevelTypes);
        }

    }

    public int getStep() {
        return step;
    }

    public void nextStep() {

        this.step++;

    }
public void fixState(){
    if (this.step >= levelTypes.get(this.level).getStepsPlayed()) {
        this.step = 0;
        this.level++;
        if((this.level % World.getWorld().NUMBEROFLEVEL ) == 0){
            this.stage++;
        }
    }
    if((this.level >= World.getWorld().NUMBEROFLEVEL * 3 )){
        this.level = 0;
        this.step = 0;
        this.stage = 0;
        World.getLogger().flushMovementFile();
    }
}

    public int getLevel() {
        return level;
    }

    public Boolean getIsCooperative(int index) {
        return levelTypes.get(index).getCooperative();
    }

    public Integer getLandscapeOrder(int index) {
        return levelTypes.get(index).getLandscapeOrder();
    }

    public Integer getStepsPlayed(int index) {
        return levelTypes.get(index).getStepsPlayed();
    }

    public ArrayList<LevelType> getLevelTypes() {
        return levelTypes;
    }

    public int getStage() { return stage; }
}

