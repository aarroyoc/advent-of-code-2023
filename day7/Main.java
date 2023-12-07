import java.nio.file.Path;
import java.io.IOException;

public class Main {
    public static void main(String[] args) throws IOException {
	var reader = new Reader(Path.of("test"));
	var game = reader.readGame();
	System.out.println(game.getTotalWinnings());
	var gamePlus = reader.readGamePlus();
	System.out.println(gamePlus.getTotalWinnings());

	reader = new Reader(Path.of("input"));
	game = reader.readGame();
	System.out.println(game.getTotalWinnings());
	gamePlus = reader.readGamePlus();
	System.out.println(gamePlus.getTotalWinnings());	
    }
}
