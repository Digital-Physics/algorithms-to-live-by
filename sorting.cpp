// Includdes Python to C++ syntax and language comparisons
#include <iostream> // a header file library that lets us work with input and output streams; import vs. #include <>
#include <vector> // see notest below on vectors vs arrays

// similar to "from lib import module" in Python, we could get standard library into the namespace to turn commands like std::cout into just cout
// using namespace std;

// types for outputs and inputs; C++ requires explicit function prototypes and a return type declaration.
// The "&" in "std::vector<int>& arr" is a pass by reference instead of by value, meaning the function won't work with a copy; it allows the function to modify the original vector directly, saves on memory (no copy), and be used by other functions.
//  In C++, a reference is an alias for an existing variable. When a function parameter is declared as a reference, any changes made to the parameter within the function will affect the original variable outside the function.
// In the context of merge, arr is a reference to a std::vector<int>. This means that the merge function can modify the contents of the original vector arr directly, and these modifications will persist outside the function.
void merge(std::vector<int>& arr, int left, int middle, int right) {
    int n1 = middle - left + 1; 
    int n2 = right - middle;

    // Vector is a sequential container to store elements and not index based. 
    // Array stores a fixed-size sequential collection of elements of the same type and it is index based. 
    // Vector is dynamic in nature so, size increases with insertion of elements. As array is fixed size, once initialized can't be resized.
    std::vector<int> leftArr(n1); // initialize size (to later assign copied values from original array), which will then be used to reassign values in arr
    std::vector<int> rightArr(n2);

    // similar to javascript for notation, but different than Python's more elegant "for i in range(n1):""
    for (int i = 0; i < n1; ++i)
        leftArr[i] = arr[left + i];
    for (int j = 0; j < n2; ++j)
        rightArr[j] = arr[middle + 1 + j];

    // that's nice; a bunch of ints initialized on one line, but it may not be best practice
    // int i = 0, j = 0;
    // alternative types for holding positive and negative integers (bytes): short (2), int (4), long (4), long long (8)
    // alternative ways to initialize a variable: int i {15} or int i{15} or int i{previous_var}
    int i = 0; // left stack starting point for merge (think top card in deck)
    int j = 0;
    int k = left; // lowest index in the lists being merged; also the index for arr

    // looks similar to javascript with && for and and if (condition) {} else {}
    // think two sorted stacks of cards. it's like a game of war (boring) to put them in order. winner goes into merge list next. loser on other stack faces next one of other stack.
    while (i < n1 && j < n2) {
        if (leftArr[i] <= rightArr[j]) {
            arr[k] = leftArr[i];
            ++i; // i++ (post-increment operator) and ++i (pre-increment operator) (not relevant here since no use or assignment in this line using i)
        } else {
            arr[k] = rightArr[j];
            ++j;
        }
        ++k; // regardless of which stack had the lower number, we increment the OG arr being filled up with final sorted values (for next round)
    }

    // only the left stack has cards/elements remaining
    while (i < n1) {
        arr[k] = leftArr[i];
        ++i;
        ++k;
    }

    while (j < n2) {
        arr[k] = rightArr[j];
        ++j;
        ++k;
    }
}

void mergeSort(std::vector<int>& arr, int left, int right) {
    if (left < right) {
        // get indices associated with splitting list in half until you get to singleton level lists (left == right)
        int middle = left + (right - left) / 2;

        // two recursive calls per call (until we hit our base case condition above), so picture a tree with each node calling two new functions
        mergeSort(arr, left, middle);
        mergeSort(arr, middle + 1, right);

        // once we get to the singleton list levels with many function calls open on the stack, we start "merging" them back together... 
        // by creating left arr and a right arr
        merge(arr, left, middle, right);
    }
}

// return 0 from the main() function is a thing in C++
// if the computer returns something other than 0, you have a problem
int main() {
    // std::vector<int> arr = {12, 11, 13, 5, 6, 7};

    // In Python, lists are dynamic arrays, and their size can change dynamically. This is not the case for C++ arrays.
    // C++ arrays have a static size determined at compile time. The size is fixed and cannot be changed during runtime.
    int array[] = {12, 11, 13, 5, 6, 7, 99, -1, 0, 0, 99}; // C++: Initializing an integer array
    // In C++, the std::vector is a dynamic array that needs to be explicitly declared and sized.
    // The vector manages its own memory. As you add or remove elements, the vector internally handles the memory allocation and deallocation. It ensures that you don't need to worry about memory management explicitly.
    // arr is the name of the vector. array points to the first element of the array (12). array + sizeof(array) / sizeof(array[0]) points one past the last element of the array.
    // In this case, sizeof(array) gives you the total size of the array in bytes. For example, if int is 4 bytes, and there are 6 elements, sizeof(array) might be 24 bytes.
    std::vector<int> arr(array, array + sizeof(array) / sizeof(array[0]));

    // character output, cout is like print in Python
    std::cout << "Original array: ";
    // C++: Range-based for loop for vectors is like "for num in arr:" in Python
    for (int num : arr) {
        std::cout << num << " "; // << is an insertion operator
    }
    // another way to get a new line, like in python, is by adding a "\n"
    // std::cout << "\n";
    std::cout << std::endl;

    // arr will be sorted in place, pass by reference defined. left index is initialized to 0 and right index is size-1, just like it indexing in Python
    mergeSort(arr, 0, arr.size() - 1); 

    std::cout << "Sorted array: ";
    for (int num : arr) {
        std::cout << num << " ";
    }
    std::cout << std::endl;

    // if the computer returns something other than 0, you have a problem
    return 0;
}
