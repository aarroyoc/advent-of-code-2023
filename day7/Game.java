import java.util.Collections;
import java.util.List;
import java.util.ArrayList;

public class Game {
    private final ArrayList<Hand> hands;

    public Game(List<Hand> hands) {
	this.hands = new ArrayList<Hand>(hands);
	this.hands.sort(Collections.reverseOrder());
    }

    public long getTotalWinnings() {
	var rank = 1;
	var total = 0;
	for(Hand hand : this.hands) {
	    total = total + (hand.getBet() * rank);
	    rank++;
	}
	return total;
    }
}
