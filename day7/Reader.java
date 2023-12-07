import java.util.regex.Pattern;
import java.util.regex.Matcher;
import java.nio.file.Path;
import java.nio.file.Files;
import java.nio.charset.StandardCharsets;
import java.io.IOException;

public class Reader {

    private final Path path;
    final static Pattern pattern = Pattern.compile("([0-9AKQJT]{5}) ([0-9]+)");
    
    public Reader(Path filename) {
	this.path = filename;
    }

    public Game readGame() throws IOException {
	var hands = Files.readAllLines(this.path, StandardCharsets.UTF_8)
	    .stream()
	    .map(line -> Reader.readLine(line))
	    .toList();
	return new Game(hands);
    }

    public GamePlus readGamePlus() throws IOException {
	var hands = Files.readAllLines(this.path, StandardCharsets.UTF_8)
	    .stream()
	    .map(line -> Reader.readLinePlus(line))
	    .toList();
	return new GamePlus(hands);
    }

    private static Hand readLine(String line) {
	Matcher matcher = Reader.pattern.matcher(line);
	if(!matcher.matches()) {
	    throw new RuntimeException("Can't parse line using RegEx");
	}
	var cards = matcher.group(1);
	var card1 = Card.of(cards.charAt(0));
        var card2 = Card.of(cards.charAt(1));
	var card3 = Card.of(cards.charAt(2));
	var card4 = Card.of(cards.charAt(3));
	var card5 = Card.of(cards.charAt(4));

	var bet = Integer.parseInt(matcher.group(2));

	return new Hand(bet, new Card[]{card1, card2, card3, card4, card5});
    }
    
    private static HandPlus readLinePlus(String line) {
	Matcher matcher = Reader.pattern.matcher(line);
	if(!matcher.matches()) {
	    throw new RuntimeException("Can't parse line using RegEx");
	}
	var cards = matcher.group(1);
	var card1 = CardPlus.of(cards.charAt(0));
        var card2 = CardPlus.of(cards.charAt(1));
	var card3 = CardPlus.of(cards.charAt(2));
	var card4 = CardPlus.of(cards.charAt(3));
	var card5 = CardPlus.of(cards.charAt(4));

	var bet = Integer.parseInt(matcher.group(2));

	return new HandPlus(bet, new CardPlus[]{card1, card2, card3, card4, card5});
    }
}
