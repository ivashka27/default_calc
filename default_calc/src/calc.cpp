#include "calc.hpp"

#include <cctype>
#include <cmath>
#include <iostream>

namespace
{

    const std::size_t max_decimal_digits = 10;

    enum class Op
    {
        ERR,
        SET,
        ADD,
        SUB,
        MUL,
        DIV,
        REM,
        NEG,
        POW,
        SQRT
    };

    std::size_t arity(const Op op)
    {
        switch (op)
        {
        case Op::ERR:
            return 0;
        case Op::NEG:
            return 1;
        case Op::SQRT:
            return 1;
        case Op::SET:
            return 2;
        case Op::ADD:
            return 2;
        case Op::SUB:
            return 2;
        case Op::MUL:
            return 2;
        case Op::DIV:
            return 2;
        case Op::REM:
            return 2;
        case Op::POW:
            return 2;
        }
        return 0;
    }

    Op parse_op(const std::string &line, std::size_t &i)
    {
        const auto rollback = [&i, &line](const std::size_t n)
        {
            i -= n;
            std::cerr << "Unknown operation " << line << std::endl;
            return Op::ERR;
        };
        switch (line[i++])
        {
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
            switch (line[i++])
            {
            case 'Q':
                switch (line[i++])
                {
                case 'R':
                    switch (line[i++])
                    {
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

    std::size_t skip_ws(const std::string &line, std::size_t i)
    {
        while (i < line.size() && std::isspace(line[i]))
            ++i;
        return i;
    }

    // Приводит символ к нижнему регистру
    char to_lower_char(char c)
    {
        return static_cast<char>(std::tolower(static_cast<unsigned char>(c)));
    }

    // Определяет основание и возвращает индекс начала цифр
    // Модифицирует i — сдвигает за префикс
    int detect_base(const std::string &line, std::size_t &i)
    {
        // Смотрим есть ли префикс 0b / 0B / 0x / 0X / ведущий 0
        if (i < line.size() && line[i] == '0')
        {
            if (i + 1 < line.size())
            {
                char next = to_lower_char(line[i + 1]);
                if (next == 'b')
                {
                    i += 2;
                    return 2;
                }
                if (next == 'x')
                {
                    i += 2;
                    return 16;
                }
                // Восьмеричное: ведущий 0 и следующий символ — цифра
                if (std::isdigit(static_cast<unsigned char>(line[i + 1])))
                {
                    i += 1;
                    return 8;
                }
            }
        }
        return 10;
    }

    // Возвращает числовое значение цифры в данном основании или -1 если недопустима
    int digit_value(char c, int base)
    {
        c = to_lower_char(c);
        int val = -1;
        if (c >= '0' && c <= '9')
            val = c - '0';
        else if (c >= 'a' && c <= 'f')
            val = 10 + (c - 'a');
        if (val >= base)
            return -1;
        return val;
    }

    double parse_arg(const std::string &line, std::size_t &i)
    {
        int base = detect_base(line, i);

        double res = 0;
        std::size_t count = 0;
        bool integer = true;
        double fraction = 1;
        bool good = true;

        while (good && i < line.size() && count < max_decimal_digits)
        {
            char c = line[i];
            if (c == '.')
            {
                integer = false;
                ++i;
                continue;
            }
            int val = digit_value(c, base);
            if (val < 0)
            {
                good = false;
                break;
            }
            if (integer)
            {
                res = res * base + val;
            }
            else
            {
                fraction /= base;
                res += val * fraction;
            }
            ++i;
            ++count;
        }
        if (!good)
        {
            std::cerr << "Argument parsing error at " << i
                      << ": '" << line.substr(i) << "'" << std::endl;
        }
        else if (i < line.size())
        {
            std::cerr << "Argument isn't fully parsed, suffix left: '"
                      << line.substr(i) << "'" << std::endl;
        }
        return res;
    }

    double unary(const double current, const Op op)
    {
        switch (op)
        {
        case Op::NEG:
            return -current;
        case Op::SQRT:
            if (current > 0)
                return std::sqrt(current);
            else
            {
                std::cerr << "Bad argument for SQRT: " << current << std::endl;
                [[fallthrough]];
            }
        default:
            return current;
        }
    }

    double binary(const Op op, const double left, const double right)
    {
        switch (op)
        {
        case Op::SET:
            return right;
        case Op::ADD:
            return left + right;
        case Op::SUB:
            return left - right;
        case Op::MUL:
            return left * right;
        case Op::DIV:
            if (right != 0)
                return left / right;
            std::cerr << "Bad right argument for division: " << right << std::endl;
            return left;
        case Op::REM:
            if (right != 0)
                return std::fmod(left, right);
            std::cerr << "Bad right argument for remainder: " << right << std::endl;
            return left;
        case Op::POW:
            return std::pow(left, right);
        default:
            return left;
        }
    }

} // anonymous namespace

double process_line(const double current, const std::string &line)
{
    std::size_t i = 0;
    const auto op = parse_op(line, i);
    switch (arity(op))
    {
    case 2:
    {
        i = skip_ws(line, i);
        const auto old_i = i;
        const auto arg = parse_arg(line, i);
        if (i == old_i)
        {
            std::cerr << "No argument for a binary operation" << std::endl;
            break;
        }
        else if (i < line.size())
        {
            break;
        }
        return binary(op, current, arg);
    }
    case 1:
    {
        if (i < line.size())
        {
            std::cerr << "Unexpected suffix for a unary operation: '"
                      << line.substr(i) << "'" << std::endl;
            break;
        }
        return unary(current, op);
    }
    default:
        break;
    }
    return current;
}