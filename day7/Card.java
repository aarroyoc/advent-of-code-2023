public enum Card {
    A,
    K,
    Q,
    J,
    T,
    NINE,
    EIGHT,
    SEVEN,
    SIX,
    FIVE,
    FOUR,
    THREE,
    TWO;

    public Card getSelf() {
	return this;
    }

    public static Card of(char code) {
	return switch(code) {
	case 'A' -> Card.A;
	case 'K' -> Card.K;
	case 'Q' -> Card.Q;
	case 'J' -> Card.J;
	case 'T' -> Card.T;
	case '9' -> Card.NINE;
	case '8' -> Card.EIGHT;
	case '7' -> Card.SEVEN;
	case '6' -> Card.SIX;
	case '5' -> Card.FIVE;
	case '4' -> Card.FOUR;
	case '3' -> Card.THREE;
	case '2' -> Card.TWO;
	default -> throw new RuntimeException("Coulnd't parse Card");
	};
    }
}
