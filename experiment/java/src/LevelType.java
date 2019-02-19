/**
 * Created by Yahosseini on 25.01.2017.
 */
public class LevelType {
        private Integer stepsPlayed ;
    private Boolean isCooperative;
    private Boolean isRich;
    private Integer landscapeOrder ;
    private Integer stage;

    public Integer getStage() {
        return stage;
    }

    public void setStage(Integer stage) {
        this.stage = stage;
    }

    public Boolean getRich() {
        return isRich;
    }

    public void setRich(Boolean smooth) {
        this.isRich = smooth;
    }

    public Integer getStepsPlayed() {
        return stepsPlayed;
    }

    public void setStepsPlayed(Integer stepsPlayed) {
        this.stepsPlayed = stepsPlayed;
    }

    public Boolean getCooperative() {
        return isCooperative;
    }

    public void setCooperative(Boolean cooperative) {
        isCooperative = cooperative;
    }

    public Integer getLandscapeOrder() {
        return landscapeOrder;
    }

    public void setLandscapeOrder(Integer landscapeOrder) {
        this.landscapeOrder = landscapeOrder;
    }


    public String toCsvString() {
        return  stepsPlayed.toString() + "," +
                isCooperative.toString()  + "," +
                isRich.toString() + "," +
                stage.toString() + ","+
                landscapeOrder.toString();
    }
}
