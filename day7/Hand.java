import java.util.Map;
import java.util.Arrays;
import java.util.stream.Collectors;

public class Hand implements Comparable<Hand> {
    private final int bet;
    private final Card[] cards;

    public enum Type {
	FIVE_OF_A_KIND,
	FOUR_OF_A_KIND,
	FULL_HOUSE,
	THREE_OF_A_KIND,
	TWO_PAIR,
	ONE_PAIR,
	HIGH_CARD
    }

    public Hand(int bet, Card[] cards) {
	this.bet = bet;
	this.cards = cards;
    }

    public int getBet() {
	return this.bet;
    }

    @Override
    public int compareTo(Hand other) {
	var type0 = this.getType();
	var type1 = other.getType();

	if(type0 == type1) {
	    // Second rule
	    for(int i=0;i<this.cards.length;i++) {
		var cmp = this.cards[i].compareTo(other.cards[i]);
		if(cmp != 0) {
		    return cmp; 
		}
	    }
	    return 0;
	} else {
	    // First rule
	    return type0.compareTo(type1);
	}
    }

    private Hand.Type getType() {
	Map<Card, Integer> count = Arrays.stream(this.cards)
	    .collect(Collectors.groupingBy(Card::getSelf, Collectors.summingInt(n -> 1)));
    	var keySet = count.keySet().toArray();

	if(count.size() == 1) {
	    return Hand.Type.FIVE_OF_A_KIND;
	} else if(count.size() == 2) {
	    var count0 = count.get(keySet[0]);
	    var count1 = count.get(keySet[1]);
	    if(count0 == 4 && count1 == 1) {
		return Hand.Type.FOUR_OF_A_KIND;
	    } else if(count0 == 1 && count1 == 4) {
		return Hand.Type.FOUR_OF_A_KIND;
	    } else if(count0 == 3 && count1 == 2) {
		return Hand.Type.FULL_HOUSE;
	    } else {
		return Hand.Type.FULL_HOUSE;
	    }
	} else if(count.size() == 3) {
	    var count0 = count.get(keySet[0]);
	    var count1 = count.get(keySet[1]);
	    var count2 = count.get(keySet[2]);

	    if(count0 == 3) {
		return Hand.Type.THREE_OF_A_KIND;
	    } else if(count1 == 3) {
                return Hand.Type.THREE_OF_A_KIND;
	    } else if(count2 == 3) {
		return Hand.Type.THREE_OF_A_KIND;
	    } else if(count0 == 2 && count1 == 2) {
		return Hand.Type.TWO_PAIR;
	    } else if(count2 == 2 && count1 == 2) {
		return Hand.Type.TWO_PAIR;
	    } else if(count0 == 2 && count2 == 2) {
		return Hand.Type.TWO_PAIR;
	    }
	} else if(count.size() == 4) {
	    return Hand.Type.ONE_PAIR;
	}
	return Hand.Type.HIGH_CARD;
    }

    @Override
    public String toString() {
	return "["+this.cards[0]+this.cards[1]+this.cards[2]+this.cards[3]+this.cards[4]+"]";
    }
}
