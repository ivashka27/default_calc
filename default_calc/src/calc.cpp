#include "calc.hpp"

#include <cctype> // for std::isspace
#include <cmath> // various math functions
#include <iostream> // for error reporting via std::cerr
#include <vector> // for left convolution

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
    // operations for left convolution
    , LADD
    , LSUB
    , LMUL
    , LDIV
    , LREM
    , LPOW
};

std::size_t arity(const Op op)
{
    switch (op) {
        case Op::ERR: return 0;
        case Op::NEG: return 1;
        case Op::SQRT: return 1;
        case Op::SET: return 2;
        case Op::ADD: return 2;
        case Op::SUB: return 2;
        case Op::MUL: return 2;
        case Op::DIV: return 2;
        case Op::REM: return 2;
        case Op::POW: return 2;
        // operations for left convolution
        case Op::LADD: return 3;
        case Op::LSUB: return 3;
        case Op::LMUL: return 3;
        case Op::LDIV: return 3;
        case Op::LREM: return 3;
        case Op::LPOW: return 3;
    }
    return 0;
}

Op parse_op(const std::string & line, std::size_t & i)
{
    if (i >= line.size()) {
        std::cerr << "Unknown operation " << line << std::endl;
        return Op::ERR;
    }

    const auto rollback = [&i, &line] (const std::size_t n) {
        i -= n;
        std::cerr << "Unknown operation " << line << std::endl;
        return Op::ERR;
    };

    const auto next_char = [&line, &i] () {
        return i < line.size() ? line[i++] : '\0';
    };

    switch (next_char()) {
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
            --i;
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
                switch (next_char()) {
                    case 'Q':
                        switch (next_char()) {
                            case 'R':
                                switch (next_char()) {
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
        case '(':
            switch (next_char())
            {
                case '+':
                    switch (next_char())
                    {
                        case ')':
                            return Op::LADD;
                        default:
                            return rollback(3);
                    }
                case '-':
                    switch (next_char())
                    {
                        case ')':
                            return Op::LSUB;
                        default:
                            return rollback(3);
                    }
                case '*':
                    switch (next_char())
                    {
                        case ')':
                            return Op::LMUL;
                        default:
                            return rollback(3);
                    }
                case '/':
                    switch (next_char())
                    {
                        case ')':
                            return Op::LDIV;
                        default:
                            return rollback(3);
                    }
                case '%':
                    switch (next_char())
                    {
                        case ')':
                            return Op::LREM;
                        default:
                            return rollback(3);
                    }
                case '^':
                    switch (next_char())
                    {
                        case ')':
                            return Op::LPOW;
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
    while (i < line.size() && std::isspace(static_cast<unsigned char>(line[i]))) {
        ++i;
    }
    return i;
}

double parse_arg(const std::string & line, std::size_t & i, const bool allow_suffix = false)
{
    if (i >= line.size()) {
        return 0;
    }

    long double res = 0;
    std::size_t count = 0;

    bool good = true;
    bool integer = true;
    bool has_space = false; // for parse_args

    int scale_of_notation = 10;
    int number_of_fraction_digits = 0;

    const std::size_t start_pos = i;

    if (line[i] == '0' && i + 1 < line.size()) {
        const char next_symbol = static_cast<char>(std::tolower(static_cast<unsigned char>(line[i + 1])));
        if (next_symbol == 'b') {
            scale_of_notation = 2;
            i += 2;
        }
        else if (next_symbol == 'x') {
            scale_of_notation = 16;
            i += 2;
        }
        else if (std::isdigit(static_cast<unsigned char>(line[i + 1]))) {
            scale_of_notation = 8;
            i += 1;
        }
    }

    while (good && i < line.size()) {
        const char current_symbol = line[i];
        int current_digit_value = -1;

        if (current_symbol >= '0' && current_symbol <= '9') {
            current_digit_value = current_symbol - '0';
        }
        else if (scale_of_notation == 16) {
            const char lower_symbol = static_cast<char>(std::tolower(static_cast<unsigned char>(current_symbol)));
            if (lower_symbol >= 'a' && lower_symbol <= 'f') {
                current_digit_value = lower_symbol - 'a' + 10;
            }
        }

        if (current_digit_value != -1 && current_digit_value < scale_of_notation) {
            if (integer) {
                res = res * scale_of_notation + current_digit_value;
            }
            else {
                number_of_fraction_digits++;
                res = res * scale_of_notation + current_digit_value;
            }
            count++;
            i++;

            if (scale_of_notation == 10 && count > max_decimal_digits) {
                std::cerr << "Argument isn't fully parsed, suffix left: '" << line.substr(i - 1) << "'" << std::endl;
                --i;
                return 0;
            }
        }
        else if (current_symbol == '.' && integer) {
            integer = false;
            i++;
        }
        else if (std::isspace(static_cast<unsigned char>(current_symbol))) {
            has_space = true;
            good = false;
        }
        else {
            good = false;
        }
    }

    if (number_of_fraction_digits > 0) {
        while (number_of_fraction_digits > 0 && std::fmod(res, static_cast<long double>(scale_of_notation)) == 0.0L) {
            res /= scale_of_notation;
            number_of_fraction_digits--;
        }
    }

    if (!good && count == 0) {
        std::cerr << "Argument parsing error at " << i << ": '" << line.substr(i) << "'" << std::endl;
        i = start_pos;
        return 0;
    }

    std::size_t temp_i = i;
    while (temp_i < line.size() && std::isspace(static_cast<unsigned char>(line[temp_i]))) {
        temp_i++;
    }

    if (count > 0 && temp_i < line.size() && !has_space) {
        std::cerr << "Argument isn't fully parsed, suffix left: '" << line.substr(temp_i) << "'" << std::endl;
        return 0;
    }

    if (!allow_suffix && temp_i < line.size()) {
        std::cerr << "Argument isn't fully parsed, suffix left: '" << line.substr(temp_i) << "'" << std::endl;
        return 0;
    }

    i = temp_i;

    double new_res = static_cast<double>(res);
    if (number_of_fraction_digits > 0) {
        new_res = static_cast<double>(res) / std::pow(static_cast<double>(scale_of_notation), number_of_fraction_digits);
    }
    return new_res;
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

std::vector<double> parse_args(const std::string & line, std::size_t & i)
{
    std::vector<double> numbers = {};

    while (i < line.size()) {
        i = skip_ws(line, i);
        if (i >= line.size()) {
            break;
        }

        const auto old_i = i;
        const auto res = parse_arg(line, i, true);

        if (old_i == i) {
            if (numbers.empty()) {
                std::cerr << "No arguments for fold operation" << std::endl;
            }
            break;
        }

        numbers.push_back(res);
    }

    if (numbers.empty()) {
        return {};
    }
    return numbers;
}

double left_convolution(const Op op, const double current, std::vector<double> & numbers)
{
    if (numbers.empty()) {
        std::cerr << "No arguments for fold operation" << std::endl;
        return current;
    }

    double res = current;
    switch (op) {
        case Op::LADD:
            for (const auto & number: numbers) {
                res = res + number;
            }
            return res;
        case Op::LSUB:
            for (const auto & number: numbers) {
                res = res - number;
            }
            return res;
        case Op::LMUL:
            for (const auto & number: numbers) {
                res = res * number;
            }
            return res;
        case Op::LDIV:
            for (const auto & number: numbers) {
                if (number != 0) {
                    res = res / number;
                }
                else {
                    std::cerr << "Bad right argument for division: " << number << std::endl;
                    return current;
                }
            }
            return res;
        case Op::LREM:
            for (const auto & number: numbers) {
                if (number != 0) {
                    res = std::fmod(res, number);
                }
                else {
                    std::cerr << "Bad right argument for remainder: " << number << std::endl;
                    return current;
                }
            }
            return res;
        case Op::LPOW:
            for (const auto & number: numbers) {
                res = std::pow(res, number);
            }
            return res;
        default:
            std::cerr << "Bad argument for convolution" << std::endl;
            return current;
    }
}

} // anonymous namespace

double process_line(const double current, const std::string & line)
{
    std::size_t i = skip_ws(line, 0);
    const auto op = parse_op(line, i);
    switch (arity(op)) {
        case 3: {
                    i = skip_ws(line, i);
                    std::vector<double> numbers = parse_args(line, i);
                    return left_convolution(op, current, numbers);
                }
        case 2: {
                    i = skip_ws(line, i);
                    const auto old_i = i;
                    const auto arg = parse_arg(line, i);
                    if (i == old_i) {
                        std::cerr << "No argument for a binary operation" << std::endl;
                        break;
                    }
                    return binary(op, current, arg);
                }
        case 1: {
                    i = skip_ws(line, i);
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
