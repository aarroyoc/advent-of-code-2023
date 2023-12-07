public enum CardPlus {
    A,
    K,
    Q,
    T,
    NINE,
    EIGHT,
    SEVEN,
    SIX,
    FIVE,
    FOUR,
    THREE,
    TWO,
    J;

    public CardPlus getSelf() {
	return this;
    }

    public static CardPlus of(char code) {
	return switch(code) {
	case 'A' -> CardPlus.A;
	case 'K' -> CardPlus.K;
	case 'Q' -> CardPlus.Q;
	case 'J' -> CardPlus.J;
	case 'T' -> CardPlus.T;
	case '9' -> CardPlus.NINE;
	case '8' -> CardPlus.EIGHT;
	case '7' -> CardPlus.SEVEN;
	case '6' -> CardPlus.SIX;
	case '5' -> CardPlus.FIVE;
	case '4' -> CardPlus.FOUR;
	case '3' -> CardPlus.THREE;
	case '2' -> CardPlus.TWO;
	default -> throw new RuntimeException("Coulnd't parse Card");
	};
    }
}
