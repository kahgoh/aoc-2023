// Version: 1.1.0

#include "vendor/unity.h"
#include "day01.h"
#include <stdio.h>

// Part 1 functions
extern int first_of_last(const char* []);
extern int find_number(const char *);
extern int sum_numbers(const char *[]);

// Part 2 functions
extern int find_number2(const char *, char * buffer);
extern int sum_numbers2(const char *[], char * buffer);
extern char sub_number(char * buffer, const char *, char replace);

void setUp(void) {
}

void tearDown(void) {
}

// --- PART 1 Tests

void test_find_12(void) {
    TEST_ASSERT_EQUAL_INT(12, find_number("1abc2"));
}

void test_find_38(void) {
    TEST_ASSERT_EQUAL_INT(38, find_number("pqr3stu8vwx"));
}

void test_find_25(void) {
    TEST_ASSERT_EQUAL_INT(15, find_number("a1b2c3d4e5f"));
}

void test_find_77(void) {
    TEST_ASSERT_EQUAL_INT(77, find_number("treb7uchet"));
}

void test_sum_numbers(void) {
    const char* strings[] = {
        "a1b2c3d4e5f",
        "treb7uchet",
        "1abc2",
        "pqr3stu8vwx",
        0,
    };
    TEST_ASSERT_EQUAL_INT(142, sum_numbers(strings));
}

void test_first_of_last(void) {
        const char* strings[] = {
        "1adtbc2",
        "pqr3stu8vwx",
        "a1b2c3d4e5f",
        0
    };
    TEST_ASSERT_EQUAL_INT('a', first_of_last(strings));
}

// Get the answer fot the input
void test_part_1(void) {
    DEF_INPUTS(inputs);

    // Assert 0 to get actual value in output
    TEST_ASSERT_EQUAL_INT(0, sum_numbers(inputs));
}

// --- Part 2 tests
void test_sub_number(void) {
    char buffer[256];
    buffer[0] = 'o';
    buffer[1] = 'n';
    buffer[2] = 'e';
    buffer[3] = 't';
    buffer[4] = 'w'; 
    buffer[5] = 'o';
    buffer[6] = 0;
    sub_number(buffer, "two", '2');
    // TEST_ASSERT_EQUAL_INT32(5, index);
    // TEST_ASSERT_EQUAL_INT(num, '2');
    TEST_ASSERT_EQUAL_STRING("onet2o", buffer);
}

void test_find2_29(void) {
    char buffer[256];
    int res = find_number2("two1nine", buffer);
    TEST_ASSERT_EQUAL_STRING("t2o1ni9e", buffer);
    TEST_ASSERT_EQUAL_INT32(29, res);
}

void test_find2_83(void) {
    char buffer[256];
    int res = find_number2("eightwothree", buffer);
    TEST_ASSERT_EQUAL_STRING("eig8t2othr3e", buffer);
    TEST_ASSERT_EQUAL_INT(83, res);
}

void test_find2_28(void) {
    char buffer[256];
    int res = find_number2("twonethreeight", buffer);
    TEST_ASSERT_EQUAL_INT(28, res);
}

void test_find2_98(void) {
    char buffer[256];
    int res = find_number2("93qd9svlnnskq8eightthree", buffer);
    TEST_ASSERT_EQUAL_INT(98, res);
}

void test_find2_42(void) {
    char buffer[256];
    int res = find_number2("jhhklldghbrlpjklpkmmjmfourxmlqhfg2jxzxbqxhdtwo", buffer);
    TEST_ASSERT_EQUAL_INT(42, res);
}

void test_find2_67(void) {
    char buffer[256];
    int res = find_number2("6817", buffer);
    TEST_ASSERT_EQUAL_INT(67, res);
}

void test_find2_66(void) {
    char buffer[256];
    int res = find_number2("vcxmllvgr6bnbssblhvhxx", buffer);
    TEST_ASSERT_EQUAL_INT(66, res);
}

void test_find2_33(void) {
    char buffer[256];
    TEST_ASSERT_EQUAL_INT(33, find_number2("3hg", buffer));
}

void test_find2_89(void) {
    char buffer[256];
    TEST_ASSERT_EQUAL_INT(89, find_number2("ccpeightbcvknglvcv81gcjnlnfnine9", buffer));
}

void test_find2_33mxm(void) {
    char buffer[256];
    TEST_ASSERT_EQUAL_INT(33, find_number2("3mxm", buffer));
}

void test_find2_33_2letter(void) {
    char buffer[256];
    TEST_ASSERT_EQUAL_INT(33, find_number2("3n", buffer));
}

void test_find2_21_joined(void) {
    char buffer[256];
    TEST_ASSERT_EQUAL_INT(21, find_number2("twone", buffer));
}

void test_find2_18_joined(void) {
    char buffer[256];
    TEST_ASSERT_EQUAL_INT(18, find_number2("oneight", buffer));
}

void test_sum_numbers2(void) {
    char buffer[256];
    const char* strings[] = {
        "two1nine",
        "eightwothree",
        "abcone2threexyz",
        "xtwone3four",
        "4nineeightseven2",
        "zoneight234",
        "7pqrstsixteen",
        0,
    };
    TEST_ASSERT_EQUAL_UINT64(281, sum_numbers2(strings, buffer));
} 

void test_sum_numbers2_broken_in(void) {
    char buffer[256];
    const char* strings[] = {
        "on2e",
        0,
    };
    TEST_ASSERT_EQUAL_UINT64(22, sum_numbers2(strings, buffer));
} 

// Test to get result for part 2
void test_part_2(void) {
    char buffer[256];
    DEF_INPUTS(inputs);

    unsigned int sum = 0;
    for (int i = 0; i < 1000; i++) {
        unsigned int num = find_number2(inputs[i], buffer);
        printf("%s: %d\n", inputs[i], num);
        sum = sum + num;
    }

    // Assert 0 to get actual value in output
    TEST_ASSERT_EQUAL_INT(0, sum);
}

int main(void) {
    UNITY_BEGIN();

    // Part 1 bits
    RUN_TEST(test_find_12);
    RUN_TEST(test_find_38);
    RUN_TEST(test_find_25);
    RUN_TEST(test_find_77);
    RUN_TEST(test_first_of_last);
    RUN_TEST(test_sum_numbers);
    RUN_TEST(test_part_1);

    // Part 2 bits
    RUN_TEST(test_sub_number);
    RUN_TEST(test_find2_29);
    RUN_TEST(test_find2_83);
    RUN_TEST(test_find2_28);
    RUN_TEST(test_find2_98);
    RUN_TEST(test_find2_42);
    RUN_TEST(test_find2_67);
    RUN_TEST(test_find2_66);
    RUN_TEST(test_find2_33);
    RUN_TEST(test_find2_89);
    RUN_TEST(test_find2_33mxm);
    RUN_TEST(test_find2_33_2letter);
    RUN_TEST(test_find2_21_joined);
    RUN_TEST(test_find2_18_joined);

    RUN_TEST(test_sum_numbers2);
    RUN_TEST(test_sum_numbers2_broken_in);
    RUN_TEST(test_part_2);

    return UNITY_END();
}
