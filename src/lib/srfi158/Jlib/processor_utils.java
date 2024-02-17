package bigloo.lib.srfi158;

public class processor_utils {

    public static int bgl_get_number_of_processors() {
        return (int)  Runtime.getRuntime().availableProcessors();
    }

}
