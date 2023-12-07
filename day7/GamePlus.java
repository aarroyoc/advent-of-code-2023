import java.util.Collections;
import java.util.List;
import java.util.ArrayList;

public class GamePlus {
    private final ArrayList<HandPlus> hands;

    public GamePlus(List<HandPlus> hands) {
	this.hands = new ArrayList<HandPlus>(hands);
	this.hands.sort(Collections.reverseOrder());
    }

    public long getTotalWinnings() {
	var rank = 1;
	var total = 0;
	for(HandPlus hand : this.hands) {
	    total = total + (hand.getBet() * rank);
	    rank++;
	}
	return total;
    }
}
