#include "calc.hpp"

#include <cctype> // for std::isspace
#include <cmath> // various math functions
#include <iostream> // for error reporting via std::cerr
#include <string>

namespace {

const std::size_t max_decimal_digits = 10;

enum class Op {
      ERR
    , SET
    , ADD
    , SUB
    , MUL
    , DIV
    , REM
    , NEG
    , POW
    , SQRT
};

std::size_t arity(const Op op)
{
    switch (op) {
        // error
        case Op::ERR: return 0;
        // unary
        case Op::NEG: return 1;
        case Op::SQRT: return 1;
        // binary
        case Op::SET: return 2;
        case Op::ADD: return 2;
        case Op::SUB: return 2;
        case Op::MUL: return 2;
        case Op::DIV: return 2;
        case Op::REM: return 2;
        case Op::POW: return 2;
    }
    return 0;
}

Op parse_op(const std::string & line, std::size_t & i)
{
    const auto rollback = [&i, &line] (const std::size_t n) {
        i -= n;
        std::cerr << "Unknown operation " << line << std::endl;
        return Op::ERR;
    };
    switch (line[i++]) {
        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':
            --i; // a first digit is a part of op's argument
            return Op::SET;
        case '+':
            return Op::ADD;
        case '-':
            return Op::SUB;
        case '*':
            return Op::MUL;
        case '/':
            return Op::DIV;
        case '%':
            return Op::REM;
        case '_':
            return Op::NEG;
        case '^':
            return Op::POW;
        case 'S':
                switch (line[i++]) {
                    case 'Q':
                        switch (line[i++]) {
                            case 'R':
                                switch (line[i++]) {
                                    case 'T':
                                        return Op::SQRT;
                                    default:
                                        return rollback(4);
                                }
                            default:
                                return rollback(3);
                        }
                    default:
                        return rollback(2);
                }
        default:
                return rollback(1);
    }
}

std::size_t skip_ws(const std::string & line, std::size_t i)
{
    while (i < line.size() && std::isspace(line[i])) {
        ++i;
    }
    return i;
}

int digit_from_char(const char c) {
    if (c >= '0' && c <= '9') {
        return c - '0';
    }
    if (c >= 'a' && c <= 'f') {
        return 10 + (c - 'a');
    }
    if (c >= 'A' && c <= 'F') {
        return 10 + (c - 'A');
    }
    return -1;
}
// При любой ошибке парсинга (повторная точка, переполнение цифр, отсутствие цифр)
// функция возвращает false и откатывает позицию. Частично накопленный результат НЕ возвращается,
// чтобы избежать ситуаций, когда пользователь ввёл некорректное число, а калькулятор
// использует его обрезанную версию.
bool parse_number_in_base(const std::string & str, std::size_t & pos, int base, double & result) {
    result = 0.0;
    bool has_digits = false;
    bool is_fractional = false;
    std::size_t integer_len = 0;
    std::size_t fractional_len = 0;
    double divisor = 1.0;
    const std::size_t start_pos = pos;

    while (pos < str.size()) {
        if (str[pos] == '.') {
            if (is_fractional) {
                std::cerr << "Ошибка парсинга числа: повторная точка в позиции " << pos << std::endl;
                pos = start_pos;
                return false;
            }
            is_fractional = true;
            ++pos;
            continue;
        }

        int digit = digit_from_char(str[pos]);
        if (digit < 0 || digit >= base) {
            break;
        }

        has_digits = true;

        if (!is_fractional) {
            if (integer_len >= max_decimal_digits) {
                std::cerr << "Ошибка: слишком много цифр в целой части (максимум " << max_decimal_digits << ")" << std::endl;
                pos = start_pos;
                return false;
            }
            result = result * base + digit;
            ++integer_len;
        } else {
            if (fractional_len >= max_decimal_digits) {
                std::cerr << "Ошибка: слишком много цифр в дробной части (максимум " << max_decimal_digits << ")" << std::endl;
                pos = start_pos;
                return false;
            }
            divisor /= base;
            result += digit * divisor;
            ++fractional_len;
        }
        ++pos;
    }

    if (!has_digits) {
        std::cerr << "Ошибка парсинга числа: цифры не найдены в позиции " << pos << std::endl;
        pos = start_pos;
        return false;
    }

    return true;
}

double parse_arg(const std::string & line, std::size_t & pos, bool & success)
{
    const std::size_t start_pos = pos;
    success = true;

    if (pos >= line.size()) {
        std::cerr << "Ошибка парсинга аргумента: пустой аргумент" << std::endl;
        success = false;
        return 0.0;
    }

    int base = 10;

    if (line[pos] == '0') {
        if (pos + 1 < line.size()) {
            if (line[pos + 1] == 'b' || line[pos + 1] == 'B') {
                base = 2;
                pos += 2;
            } else if (line[pos + 1] == 'x' || line[pos + 1] == 'X') {
                base = 16;
                pos += 2;
            } else if (line[pos + 1] != '.' && line[pos+1] <= '7') {
                base = 8;
            }
        }
    }

    const std::size_t digits_start = pos;
    double result = 0.0;
    const bool parse_ok = parse_number_in_base(line, pos, base, result);

    if (!parse_ok || pos == digits_start) {
        std::cerr << "Ошибка парсинга аргумента: неверный формат числа" << std::endl;
        pos = start_pos;
        success = false;
        return 0.0;
    }

    if (pos == start_pos) {
        std::cerr << "Ошибка парсинга аргумента в позиции " << pos << std::endl;
        success = false;
        return 0.0;
    }

    return result;
}

double unary(const double current, const Op op)
{
    switch (op) {
        case Op::NEG:
            return -current;
        case Op::SQRT:
            if (current >= 0) {
                return std::sqrt(current);
            }
            else {
                std::cerr << "Bad argument for SQRT: " << current << std::endl;
                [[fallthrough]];
            }
        default:
            return current;
    }
}

double binary(const Op op, const double left, const double right)
{
    switch (op) {
        case Op::SET:
            return right;
        case Op::ADD:
            return left + right;
        case Op::SUB:
            return left - right;
        case Op::MUL:
            return left * right;
        case Op::DIV:
            if (right != 0) {
                return left / right;
            }
            else {
                std::cerr << "Bad right argument for division: " << right << std::endl;
                return left;
            }
        case Op::REM:
            if (right != 0) {
                return std::fmod(left, right);
            }
            else {
                std::cerr << "Bad right argument for remainder: " << right << std::endl;
                return left;
            }
        case Op::POW:
            return std::pow(left, right);
        default:
            return left;
    }
}

} // anonymous namespace

double process_line(const double current, const std::string & line)
{
    if (line.empty()) {
        std::cerr << "Empty input" << std::endl;
        return current;
    }

    std::size_t i = skip_ws(line, 0);
    if (i >= line.size()) {
        std::cerr << "Empty input (only whitespace)" << std::endl;
        return current;
    }

    // Проверка на свёртку: строка начинается с '('
    if (line[i] == '(') {
        std::size_t pos = i + 1;

        const Op fold_op = parse_op(line, pos);
        if (fold_op == Op::ERR) {
            std::cerr << "Ошибка: неизвестный оператор в свёртке" << std::endl;
            return current;
        }

        if (arity(fold_op) != 2) {
            std::cerr << "Ошибка: для свёртки нужна бинарная операция" << std::endl;
            return current;
        }

        pos = skip_ws(line, pos);
        if (pos >= line.size() || line[pos] != ')') {
            std::cerr << "Ошибка: ожидалась ')' после оператора в свёртке" << std::endl;
            return current;
        }
        ++pos;

        double result = current;
        while (pos < line.size()) {
            pos = skip_ws(line, pos);
            if (pos >= line.size()) break;

            const std::size_t old_pos = pos;

            bool success = true;
            const double arg = parse_arg(line, pos, success);
            if (!success || pos == old_pos) {
                std::cerr << "Ошибка: ожидалось число в свёртке" << std::endl;
                break;
            }
            result = binary(fold_op, result, arg);
        }
        return result;
    }

    // Обычная обработка (без свёртки)
    const auto op = parse_op(line, i);
    switch (arity(op)) {
        case 2: {
                    i = skip_ws(line, i);
                    const auto old_i = i;
                    bool success = true;
                    const auto arg = parse_arg(line, i, success);
                    if (!success || i == old_i) {
                        std::cerr << "No argument for a binary operation" << std::endl;
                        break;
                    }
                    else if (i < line.size()) {
                        std::cerr << "Ошибка: обнаружен мусор после аргумента" << std::endl;
                        break;
                    }
                    return binary(op, current, arg);
                }
        case 1: {
                    if (i < line.size()) {
                        std::cerr << "Unexpected suffix for a unary operation: '" << line.substr(i) << "'" << std::endl;
                        break;
                    }
                    return unary(current, op);
                }
        default: break;
    }
    return current;
}