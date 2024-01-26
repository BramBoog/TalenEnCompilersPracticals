class Test {
    int d(int x){
        return x + 1;
    }

    void main()
    {
        int a;
        for (int i, i = 0; i < 2; i = i + 1) {
            a = a + d(i);
        }
        print(a);
	}
}
