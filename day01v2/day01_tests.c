// Version: 1.1.0

#include "vendor/unity.h"
#include "day01.h"
#include <stdio.h>

// Part 1 functions
extern int find_digit(const char*, const char *);
extern int find_number(const char*);
extern int play_test();
extern int sum_numbers(const char *[]);

void setUp(void) {
}

void tearDown(void) {
}

void test_find_digit_1(void) {
    TEST_ASSERT_EQUAL_INT(0x10202, find_digit("oo1pp", "1"));
}

void test_find_digit_2_double(void) {
    TEST_ASSERT_EQUAL_INT(0x10103, find_digit("o212p", "2"));
}

void test_find_digit_double_letter(void) {
    TEST_ASSERT_EQUAL_INT(0x10303, find_digit("ttwo", "two"));
}

void test_find_absent_digit(void) {
    TEST_ASSERT_EQUAL_INT(0x00000, find_digit("o212p", "3"));
}

void test_find_word(void) {
    TEST_ASSERT_EQUAL_INT(0x10303, find_digit("2one4", "one"));
}

void test_find_number_29(void) {
    TEST_ASSERT_EQUAL_INT(29, find_number("two1nine"));
}

void test_find_number_14(void) {
    TEST_ASSERT_EQUAL_INT(14, find_number("zonesight2one34"));
}

void test_find_number_83(void) {
    TEST_ASSERT_EQUAL_INT(83, find_number("eig8t2othr3e"));
}

void test_find_number_93(void) {
    TEST_ASSERT_EQUAL_INT(93, find_number("93qd9svlnnskq8eightthree"));
}

void test_find_number_42(void) {
    TEST_ASSERT_EQUAL_INT(42, find_number("jhhklldghbrlpjklpkmmjmfourxmlqhfg2jxzxbqxhdtwo"));
}

void test_find_number_67(void) {
    TEST_ASSERT_EQUAL_INT(67, find_number("6817"));
}

void test_find_number_single_digit_at_start(void) {
    TEST_ASSERT_EQUAL_INT(22, find_number("2cvtbpsbbhbzrxmp"));
}

void test_find_number_single_digit_at_end(void) {
    TEST_ASSERT_EQUAL_INT(44, find_number("x4"));
}

void test_find_number_single_word_at_start(void) {
    TEST_ASSERT_EQUAL_INT(99, find_number("ninety"));
}

void test_find_number_single_word_at_end(void) {
    TEST_ASSERT_EQUAL_INT(66, find_number("zsix"));
}

void test_find_number_digit_double_letter_53(void) {
    TEST_ASSERT_EQUAL_INT(53, find_number("5tthree"));
}

void test_find_number_digit_double_word_58(void) {
    TEST_ASSERT_EQUAL_INT(58, find_number("5xxxthreeeight"));
}

void test_sum_numbers(void) {
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
    TEST_ASSERT_EQUAL_INT(281, sum_numbers(strings));
}

void test_sum_inputs(void) {
    DEF_INPUTS(inputs);
    TEST_ASSERT_EQUAL_INT(0, sum_numbers(inputs));
}

void test_part_2(void) {
    DEF_INPUTS(inputs);

    unsigned int sum = 0;
    for (int i = 0; i < 1000; i++) {
        unsigned int num = find_number(inputs[i]);
        printf("%s: %d\n", inputs[i], num);
        sum = sum + num;
    }

    // Assert 0 to get actual value in output
    TEST_ASSERT_EQUAL_INT(0, sum);
}

int main(void) {
    UNITY_BEGIN();

    // RUN_TEST(test_part_2);
    // return 0;

    RUN_TEST(test_find_digit_1);
    RUN_TEST(test_find_digit_2_double);
    RUN_TEST(test_find_digit_double_letter);
    RUN_TEST(test_find_absent_digit);
    RUN_TEST(test_find_word);
    RUN_TEST(test_find_number_29);
    RUN_TEST(test_find_number_14);
    RUN_TEST(test_find_number_83);
    RUN_TEST(test_find_number_93);
    RUN_TEST(test_find_number_42);
    RUN_TEST(test_find_number_67);
    RUN_TEST(test_find_number_single_digit_at_start);
    RUN_TEST(test_find_number_single_digit_at_end);
    RUN_TEST(test_find_number_single_word_at_start);
    RUN_TEST(test_find_number_single_word_at_end);
    RUN_TEST(test_find_number_digit_double_letter_53);
    RUN_TEST(test_find_number_digit_double_word_58);


    // RUN_TEST(test_sum_numbers);
    RUN_TEST(test_sum_inputs);

    return UNITY_END();
}
