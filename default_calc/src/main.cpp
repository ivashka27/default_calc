#include "calc.hpp"

#include <iostream>
#include <string>
#include <iomanip>
#include <cmath>

int main()
{
    double current = 0;

    for (std::string line; std::getline(std::cin, line); ) {
        current = process_line(current, line);

        double int_part;
        if (std::modf(current, &int_part) == 0.0) {
            std::cout << std::fixed << std::setprecision(0) << current << std::endl;
        }
        else {
            std::cout << std::fixed << std::setprecision(6) << current << std::endl;
        }
    }
}
// Проверка на то, дробное число или нет, чтобы корректно выводить целые и дробные числа (при запросе 13 выведет 13, а не 13.00000)