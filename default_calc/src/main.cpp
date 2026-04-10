#include <iostream>
#include <string>
#include <iomanip>
#include "calc.hpp"

int main() {
    double reg = 0.0;
    std::string line;

    while (std::getline(std::cin, line)) {
        reg = process_line(reg, line);
        std::cout << std::fixed << std::setprecision(6) << reg << std::endl;
    }

    return 0;
}
