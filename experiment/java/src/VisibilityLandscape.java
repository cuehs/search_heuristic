public interface VisibilityLandscape {
    void makeVisibilityLandscape(int edgeSize);

    void resetVisibilityLandscapeAndPosition();

    void resetVisibilityLandscape();

    int getPosition();

    void setPosition(int position);

    void startPosition();

    void makeVisibleBySelf(int position);

    void makeVisibleByOther(int position);
}
