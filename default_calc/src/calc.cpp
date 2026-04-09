#include "calc.hpp"

#include <cctype>   // for std::isspace
#include <cmath>    // various math functions
#include <iostream> // for error reporting via std::cerr
#include <utility>  // for std::pair

namespace {

const std::size_t max_decimal_digits = 10;

enum class Op {
  ERR,
  SET,
  ADD,
  SUB,
  MUL,
  DIV,
  REM,
  NEG,
  POW,
  SQRT,
  SIN,
  COS,
  TAN,
  CTN,
  RAD,
  DEG,
  ASIN,
  ACOS,
  ATAN,
  ACTN
};

std::size_t arity(const Op op) {
  switch (op) {
  // error
  case Op::ERR:
    return 0;
  // unary
  case Op::NEG:
  case Op::SQRT:
  case Op::SIN:
  case Op::COS:
  case Op::TAN:
  case Op::CTN:
  case Op::RAD:
  case Op::DEG:
  case Op::ASIN:
  case Op::ACOS:
  case Op::ATAN:
  case Op::ACTN:
    return 1;
  // binary
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

// using pair with an index because it is being silently mutated inside
std::pair<Op, std::size_t> parse_op(const std::string &line, std::size_t i) {
  if (i >= line.size()) {
    std::cerr << "Unknown operation " << line << std::endl;
    return {Op::ERR, i};
  }

  const char c = line[i];
  if (c >= '0' && c <= '9') {
    return {Op::SET, i};
  }

  switch (c) {
  case '+':
    return {Op::ADD, i + 1};
  case '-':
    return {Op::SUB, i + 1};
  case '*':
    return {Op::MUL, i + 1};
  case '/':
    return {Op::DIV, i + 1};
  case '%':
    return {Op::REM, i + 1};
  case '_':
    return {Op::NEG, i + 1};
  case '^':
    return {Op::POW, i + 1};
  }

  // to avoid ugly nesting with switch statements
  if (line.compare(i, 4, "SQRT") == 0)
    return {Op::SQRT, i + 4};
  if (line.compare(i, 4, "ASIN") == 0)
    return {Op::ASIN, i + 4};
  if (line.compare(i, 4, "ACOS") == 0)
    return {Op::ACOS, i + 4};
  if (line.compare(i, 4, "ATAN") == 0)
    return {Op::ATAN, i + 4};
  if (line.compare(i, 4, "ACTN") == 0)
    return {Op::ACTN, i + 4};

  if (line.compare(i, 3, "SIN") == 0)
    return {Op::SIN, i + 3};
  if (line.compare(i, 3, "COS") == 0)
    return {Op::COS, i + 3};
  if (line.compare(i, 3, "TAN") == 0)
    return {Op::TAN, i + 3};
  if (line.compare(i, 3, "CTN") == 0)
    return {Op::CTN, i + 3};
  if (line.compare(i, 3, "RAD") == 0)
    return {Op::RAD, i + 3};
  if (line.compare(i, 3, "DEG") == 0)
    return {Op::DEG, i + 3};

  std::cerr << "Unknown operation " << line << std::endl;
  return {Op::ERR, i};
}

// note that spaces are only skipped in the middle
// e.g. "      5" is invalid and "+      5" is valid
std::size_t skip_ws(const std::string &line, std::size_t i) {
  while (i < line.size() && std::isspace(line[i])) {
    ++i;
  }
  return i;
}

std::pair<double, std::size_t> parse_arg(const std::string &line,
                                         std::size_t i) {
  double res = 0;
  std::size_t count = 0;
  bool good = true;
  bool integer = true;
  double fraction = 1;
  while (good && i < line.size() && count < max_decimal_digits) {
    switch (line[i]) {
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
      if (integer) {
        res *= 10;
        res += line[i] - '0';
      } else {
        fraction /= 10;
        res += (line[i] - '0') * fraction;
      }
      ++i;
      ++count;
      break;
    case '.':
      integer = false;
      ++i;
      break;
    default:
      good = false;
      break;
    }
  }
  if (!good) {
    std::cerr << "Argument parsing error at " << i << ": '" << line.substr(i)
              << "'" << std::endl;
  } else if (i < line.size()) {
    std::cerr << "Argument isn't fully parsed, suffix left: '" << line.substr(i)
              << "'" << std::endl;
  }
  return {res, i};
}

double unary(const double current, bool &rad_on, const Op op) {
  // added for reusability
  auto cout_bad_argument = [current](std::string name) {
    std::cerr << "Bad argument for " << name << ": " << current << std::endl;
    return current;
  };

  // There is no way to further simplify the switch code
  // because of the considerations: radians, available cmath, boundaries
  auto to_radians = [](double deg) { return deg * M_PI / 180.0; };
  auto to_degrees = [](double rad) { return rad * 180.0 / M_PI; };

  double angle_rad = rad_on ? current : to_radians(current);

  switch (op) {
  case Op::NEG:
    return -current;
  case Op::SQRT:
    if (current >= 0) {
      return std::sqrt(current);
    } else {
      return cout_bad_argument("SQRT");
    }
  case Op::RAD:
    rad_on = true;
    return current;
  case Op::DEG:
    rad_on = false;
    return current;
  case Op::SIN:
    return std::sin(angle_rad);
  case Op::COS:
    return std::cos(angle_rad);
  case Op::TAN:
    if (std::abs(std::cos(angle_rad)) < 1e-10) {
      return cout_bad_argument("TAN");
    }
    return std::tan(angle_rad);
  case Op::CTN:
    if (std::abs(std::sin(angle_rad)) < 1e-10) {
      return cout_bad_argument("CTN");
    }
    return 1.0 / std::tan(angle_rad);
  case Op::ASIN:
    if (current < -1.0 || current > 1.0)
      return cout_bad_argument("ASIN");
    return rad_on ? std::asin(current) : to_degrees(std::asin(current));
  case Op::ACOS:
    if (current < -1.0 || current > 1.0)
      return cout_bad_argument("ACOS");
    return rad_on ? std::acos(current) : to_degrees(std::acos(current));
  case Op::ATAN:
    return rad_on ? std::atan(current) : to_degrees(std::atan(current));
  case Op::ACTN:
    return rad_on ? (M_PI / 2.0 - std::atan(current))
                  : to_degrees(M_PI / 2.0 - std::atan(current));
  default:
    return current;
  }
}

double binary(const Op op, const double left, const double right) {
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
    } else {
      std::cerr << "Bad right argument for division: " << right << std::endl;
      return left;
    }
  case Op::REM:
    if (right != 0) {
      return std::fmod(left, right);
    } else {
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

double process_line(const double current, bool &rad_on,
                    const std::string &line) {
  auto [op, i] = parse_op(line, 0);
  switch (arity(op)) {
  case 2: {
    i = skip_ws(line, i);
    const auto old_i = i;
    const auto [arg, next_i] = parse_arg(line, i);
    i = next_i;
    if (i == old_i) {
      std::cerr << "No argument for a binary operation" << std::endl;
      break;
    } else if (i < line.size()) {
      break;
    }
    return binary(op, current, arg);
  }
  case 1: {
    if (i < line.size()) {
      std::cerr << "Unexpected suffix for a unary operation: '"
                << line.substr(i) << "'" << std::endl;
      break;
    }
    return unary(current, rad_on, op);
  }
  default:
    break;
  }
  return current;
}