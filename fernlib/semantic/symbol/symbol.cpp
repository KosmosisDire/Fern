#include "symbol.hpp"

namespace Fern
{

std::string NamedTypeSymbol::format(int indent) const
{
    std::ostringstream ss;
    std::string pad(indent, ' ');
    ss << pad << "type " << name;
    if (!fields.empty() || !methods.empty() || !nestedTypes.empty())
    {
        ss << "\n" << pad << "{\n";
        for (const auto& field : fields)
        {
            ss << field->format(indent + 4) << "\n";
        }
        for (const auto& method : methods)
        {
            ss << method->format(indent + 4) << "\n";
        }
        for (const auto& nested : nestedTypes)
        {
            ss << nested->format(indent + 4) << "\n";
        }
        ss << pad << "}";
    }
    return ss.str();
}

std::string MethodSymbol::format(int indent) const
{
    std::ostringstream ss;
    std::string pad(indent, ' ');
    std::string retName = returnType ? returnType->name : "void";
    ss << pad << "fn " << name << "() -> " << retName;
    if (!parameters.empty())
    {
        ss << "\n" << pad << "{\n";
        for (const auto& param : parameters)
        {
            ss << param->format(indent + 4) << "\n";
        }
        ss << pad << "}";
    }
    return ss.str();
}

}
