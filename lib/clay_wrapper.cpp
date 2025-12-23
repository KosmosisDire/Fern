// Clay wrapper for Fern FFI
// This file provides a simpler C API that Fern can call via extern declarations

#define CLAY_IMPLEMENTATION
#include "clay.h"

#include <cstdlib>
#include <cstring>

extern "C" {

// ============================================================================
// Memory and Initialization
// ============================================================================

static Clay_Arena g_arena;
static Clay_Context* g_context = nullptr;
static void* g_arena_memory = nullptr;

// Error handler that stores the last error
static char g_last_error[1024] = {0};

void Clay_ErrorCallback(Clay_ErrorData errorData) {
    if (errorData.errorText.chars && errorData.errorText.length > 0) {
        int len = errorData.errorText.length < 1023 ? errorData.errorText.length : 1023;
        memcpy(g_last_error, errorData.errorText.chars, len);
        g_last_error[len] = '\0';
    }
}

// Initialize Clay with default settings
// Returns true on success
bool Fern_Clay_Init(float width, float height) {
    if (g_context != nullptr) {
        return true; // Already initialized
    }

    uint32_t memSize = Clay_MinMemorySize();
    g_arena_memory = malloc(memSize);
    if (!g_arena_memory) {
        return false;
    }

    g_arena = Clay_CreateArenaWithCapacityAndMemory(memSize, g_arena_memory);
    Clay_Dimensions dims = { width, height };
    Clay_ErrorHandler errorHandler = { Clay_ErrorCallback, nullptr };

    g_context = Clay_Initialize(g_arena, dims, errorHandler);
    return g_context != nullptr;
}

// Shutdown Clay and free memory
void Fern_Clay_Shutdown() {
    if (g_arena_memory) {
        free(g_arena_memory);
        g_arena_memory = nullptr;
    }
    g_context = nullptr;
}

// Get the last error message
const char* Fern_Clay_GetLastError() {
    return g_last_error;
}

// ============================================================================
// Frame Lifecycle
// ============================================================================

void Fern_Clay_SetLayoutDimensions(float width, float height) {
    Clay_SetLayoutDimensions((Clay_Dimensions){ width, height });
}

void Fern_Clay_SetPointerState(float x, float y, bool isDown) {
    Clay_SetPointerState((Clay_Vector2){ x, y }, isDown);
}

void Fern_Clay_UpdateScrollContainers(bool enableDrag, float scrollX, float scrollY, float deltaTime) {
    Clay_UpdateScrollContainers(enableDrag, (Clay_Vector2){ scrollX, scrollY }, deltaTime);
}

void Fern_Clay_BeginLayout() {
    Clay_BeginLayout();
}

// ============================================================================
// Render Commands - returned from EndLayout
// ============================================================================

static Clay_RenderCommandArray g_renderCommands;

int32_t Fern_Clay_EndLayout() {
    g_renderCommands = Clay_EndLayout();
    return g_renderCommands.length;
}

int32_t Fern_Clay_GetRenderCommandCount() {
    return g_renderCommands.length;
}

// Get render command type (returns Clay_RenderCommandType as int)
int32_t Fern_Clay_GetRenderCommandType(int32_t index) {
    if (index < 0 || index >= g_renderCommands.length) return 0;
    return (int32_t)g_renderCommands.internalArray[index].commandType;
}

// Get bounding box for a render command
void Fern_Clay_GetRenderCommandBounds(int32_t index, float* x, float* y, float* width, float* height) {
    if (index < 0 || index >= g_renderCommands.length) {
        *x = *y = *width = *height = 0;
        return;
    }
    Clay_BoundingBox box = g_renderCommands.internalArray[index].boundingBox;
    *x = box.x;
    *y = box.y;
    *width = box.width;
    *height = box.height;
}

// Get rectangle render data
void Fern_Clay_GetRectangleData(int32_t index, float* r, float* g, float* b, float* a,
                                 float* cornerTL, float* cornerTR, float* cornerBL, float* cornerBR) {
    if (index < 0 || index >= g_renderCommands.length) {
        *r = *g = *b = *a = 0;
        *cornerTL = *cornerTR = *cornerBL = *cornerBR = 0;
        return;
    }
    Clay_RenderCommand* cmd = &g_renderCommands.internalArray[index];
    Clay_RectangleRenderData rect = cmd->renderData.rectangle;
    *r = rect.backgroundColor.r;
    *g = rect.backgroundColor.g;
    *b = rect.backgroundColor.b;
    *a = rect.backgroundColor.a;
    *cornerTL = rect.cornerRadius.topLeft;
    *cornerTR = rect.cornerRadius.topRight;
    *cornerBL = rect.cornerRadius.bottomLeft;
    *cornerBR = rect.cornerRadius.bottomRight;
}

// Get text render data
const char* Fern_Clay_GetTextData(int32_t index, int32_t* length, float* r, float* g, float* b, float* a,
                                   uint16_t* fontId, uint16_t* fontSize) {
    if (index < 0 || index >= g_renderCommands.length) {
        *length = 0;
        *r = *g = *b = *a = 0;
        *fontId = *fontSize = 0;
        return nullptr;
    }
    Clay_RenderCommand* cmd = &g_renderCommands.internalArray[index];
    Clay_TextRenderData text = cmd->renderData.text;
    *length = text.stringContents.length;
    *r = text.textColor.r;
    *g = text.textColor.g;
    *b = text.textColor.b;
    *a = text.textColor.a;
    *fontId = text.fontId;
    *fontSize = text.fontSize;
    return text.stringContents.chars;
}

// Get border render data
void Fern_Clay_GetBorderData(int32_t index, float* r, float* g, float* b, float* a,
                              uint16_t* left, uint16_t* right, uint16_t* top, uint16_t* bottom) {
    if (index < 0 || index >= g_renderCommands.length) {
        *r = *g = *b = *a = 0;
        *left = *right = *top = *bottom = 0;
        return;
    }
    Clay_RenderCommand* cmd = &g_renderCommands.internalArray[index];
    Clay_BorderRenderData border = cmd->renderData.border;
    *r = border.color.r;
    *g = border.color.g;
    *b = border.color.b;
    *a = border.color.a;
    *left = border.width.left;
    *right = border.width.right;
    *top = border.width.top;
    *bottom = border.width.bottom;
}

// ============================================================================
// Element Declaration - Simple API
// ============================================================================

void Fern_Clay_OpenElement() {
    Clay__OpenElement();
}

void Fern_Clay_CloseElement() {
    Clay__CloseElement();
}

// Configure the currently open element with an ID
void Fern_Clay_SetElementId(const char* id) {
    Clay_ElementDeclaration decl = {};
    if (id) {
        Clay_String str = { true, (int32_t)strlen(id), id };
        decl.id = Clay__HashString(str, 0, 0);
    }
    Clay__ConfigureOpenElement(decl);
}

// Full element configuration function
void Fern_Clay_ConfigureElement(
    // ID
    const char* id,
    // Layout
    float sizingWidthMin, float sizingWidthMax, int32_t sizingWidthType,
    float sizingHeightMin, float sizingHeightMax, int32_t sizingHeightType,
    uint16_t paddingLeft, uint16_t paddingRight, uint16_t paddingTop, uint16_t paddingBottom,
    uint16_t childGap,
    int32_t layoutDirection,  // 0 = LEFT_TO_RIGHT, 1 = TOP_TO_BOTTOM
    int32_t alignX,           // 0 = LEFT, 1 = CENTER, 2 = RIGHT
    int32_t alignY,           // 0 = TOP, 1 = CENTER, 2 = BOTTOM
    // Background color
    float bgR, float bgG, float bgB, float bgA,
    // Corner radius
    float cornerTL, float cornerTR, float cornerBL, float cornerBR
) {
    Clay_ElementDeclaration decl = {};

    // ID
    if (id) {
        Clay_String str = { true, (int32_t)strlen(id), id };
        decl.id = Clay__HashString(str, 0, 0);
    }

    // Sizing width
    decl.layout.sizing.width.size.minMax.min = sizingWidthMin;
    decl.layout.sizing.width.size.minMax.max = sizingWidthMax;
    decl.layout.sizing.width.type = (Clay__SizingType)sizingWidthType;

    // Sizing height
    decl.layout.sizing.height.size.minMax.min = sizingHeightMin;
    decl.layout.sizing.height.size.minMax.max = sizingHeightMax;
    decl.layout.sizing.height.type = (Clay__SizingType)sizingHeightType;

    // Padding
    decl.layout.padding.left = paddingLeft;
    decl.layout.padding.right = paddingRight;
    decl.layout.padding.top = paddingTop;
    decl.layout.padding.bottom = paddingBottom;

    // Child gap and direction
    decl.layout.childGap = childGap;
    decl.layout.layoutDirection = (Clay_LayoutDirection)layoutDirection;
    decl.layout.childAlignment.x = (Clay_LayoutAlignmentX)alignX;
    decl.layout.childAlignment.y = (Clay_LayoutAlignmentY)alignY;

    // Background color
    decl.backgroundColor = { bgR, bgG, bgB, bgA };

    // Corner radius
    decl.cornerRadius = { cornerTL, cornerTR, cornerBL, cornerBR };

    Clay__ConfigureOpenElement(decl);
}

// Simplified element with just background color
void Fern_Clay_ConfigureRect(const char* id, float r, float g, float b, float a) {
    Clay_ElementDeclaration decl = {};
    if (id) {
        Clay_String str = { true, (int32_t)strlen(id), id };
        decl.id = Clay__HashString(str, 0, 0);
    }
    decl.backgroundColor = { r, g, b, a };
    Clay__ConfigureOpenElement(decl);
}

// ============================================================================
// Text Elements
// ============================================================================

static Clay_TextElementConfig g_textConfig;

void Fern_Clay_Text(const char* text, uint16_t fontId, uint16_t fontSize,
                     float r, float g, float b, float a) {
    if (!text) return;

    g_textConfig.fontId = fontId;
    g_textConfig.fontSize = fontSize;
    g_textConfig.textColor = { r, g, b, a };
    g_textConfig.wrapMode = CLAY_TEXT_WRAP_WORDS;

    Clay_String str = { true, (int32_t)strlen(text), text };
    Clay__OpenTextElement(str, &g_textConfig);
}

void Fern_Clay_TextAdvanced(const char* text, uint16_t fontId, uint16_t fontSize,
                             float r, float g, float b, float a,
                             uint16_t letterSpacing, uint16_t lineHeight, int32_t wrapMode) {
    if (!text) return;

    g_textConfig.fontId = fontId;
    g_textConfig.fontSize = fontSize;
    g_textConfig.textColor = { r, g, b, a };
    g_textConfig.letterSpacing = letterSpacing;
    g_textConfig.lineHeight = lineHeight;
    g_textConfig.wrapMode = (Clay_TextElementConfigWrapMode)wrapMode;

    Clay_String str = { true, (int32_t)strlen(text), text };
    Clay__OpenTextElement(str, &g_textConfig);
}

// ============================================================================
// Text Measurement Callback
// ============================================================================

typedef void (*FernMeasureTextCallback)(const char* text, int32_t length, uint16_t fontId,
                                         uint16_t fontSize, float* outWidth, float* outHeight);

static FernMeasureTextCallback g_fernMeasureCallback = nullptr;

Clay_Dimensions Fern_Clay_MeasureTextInternal(Clay_StringSlice text, Clay_TextElementConfig* config, void* userData) {
    (void)userData;
    if (g_fernMeasureCallback && text.chars && text.length > 0) {
        float width = 0, height = 0;
        g_fernMeasureCallback(text.chars, text.length, config->fontId, config->fontSize, &width, &height);
        return { width, height };
    }
    // Fallback: approximate with fixed width per character
    return { (float)(text.length * config->fontSize * 0.6f), (float)config->fontSize };
}

void Fern_Clay_SetMeasureTextCallback(FernMeasureTextCallback callback) {
    g_fernMeasureCallback = callback;
    Clay_SetMeasureTextFunction(Fern_Clay_MeasureTextInternal, nullptr);
}

// ============================================================================
// Interaction
// ============================================================================

bool Fern_Clay_Hovered() {
    return Clay_Hovered();
}

bool Fern_Clay_PointerOver(const char* id) {
    if (!id) return false;
    Clay_String str = { true, (int32_t)strlen(id), id };
    Clay_ElementId elemId = Clay__HashString(str, 0, 0);
    return Clay_PointerOver(elemId);
}

// Get scroll offset for an element
void Fern_Clay_GetScrollOffset(float* x, float* y) {
    Clay_Vector2 offset = Clay_GetScrollOffset();
    *x = offset.x;
    *y = offset.y;
}

// ============================================================================
// Floating Elements
// ============================================================================

void Fern_Clay_ConfigureFloating(
    const char* id,
    float offsetX, float offsetY,
    int32_t attachTo,      // 0=NONE, 1=PARENT, 2=ELEMENT_ID, 3=ROOT
    int32_t attachElement, // 0=LEFT_TOP ... 8=RIGHT_BOTTOM
    int32_t attachParent,
    int16_t zIndex
) {
    Clay_ElementDeclaration decl = {};

    if (id) {
        Clay_String str = { true, (int32_t)strlen(id), id };
        decl.id = Clay__HashString(str, 0, 0);
    }

    decl.floating.offset = { offsetX, offsetY };
    decl.floating.attachTo = (Clay_FloatingAttachToElement)attachTo;
    decl.floating.attachPoints.element = (Clay_FloatingAttachPointType)attachElement;
    decl.floating.attachPoints.parent = (Clay_FloatingAttachPointType)attachParent;
    decl.floating.zIndex = zIndex;

    Clay__ConfigureOpenElement(decl);
}

// ============================================================================
// Clipping / Scrolling
// ============================================================================

void Fern_Clay_ConfigureClip(const char* id, bool horizontal, bool vertical, float offsetX, float offsetY) {
    Clay_ElementDeclaration decl = {};

    if (id) {
        Clay_String str = { true, (int32_t)strlen(id), id };
        decl.id = Clay__HashString(str, 0, 0);
    }

    decl.clip.horizontal = horizontal;
    decl.clip.vertical = vertical;
    decl.clip.childOffset = { offsetX, offsetY };

    Clay__ConfigureOpenElement(decl);
}

// ============================================================================
// Border Configuration
// ============================================================================

void Fern_Clay_ConfigureBorder(
    const char* id,
    float r, float g, float b, float a,
    uint16_t left, uint16_t right, uint16_t top, uint16_t bottom, uint16_t between
) {
    Clay_ElementDeclaration decl = {};

    if (id) {
        Clay_String str = { true, (int32_t)strlen(id), id };
        decl.id = Clay__HashString(str, 0, 0);
    }

    decl.border.color = { r, g, b, a };
    decl.border.width = { left, right, top, bottom, between };

    Clay__ConfigureOpenElement(decl);
}

// ============================================================================
// Debug
// ============================================================================

void Fern_Clay_SetDebugMode(bool enabled) {
    Clay_SetDebugModeEnabled(enabled);
}

bool Fern_Clay_IsDebugMode() {
    return Clay_IsDebugModeEnabled();
}

// ============================================================================
// Render Command Type Constants
// ============================================================================

int32_t Fern_Clay_RenderType_None() { return CLAY_RENDER_COMMAND_TYPE_NONE; }
int32_t Fern_Clay_RenderType_Rectangle() { return CLAY_RENDER_COMMAND_TYPE_RECTANGLE; }
int32_t Fern_Clay_RenderType_Border() { return CLAY_RENDER_COMMAND_TYPE_BORDER; }
int32_t Fern_Clay_RenderType_Text() { return CLAY_RENDER_COMMAND_TYPE_TEXT; }
int32_t Fern_Clay_RenderType_Image() { return CLAY_RENDER_COMMAND_TYPE_IMAGE; }
int32_t Fern_Clay_RenderType_ScissorStart() { return CLAY_RENDER_COMMAND_TYPE_SCISSOR_START; }
int32_t Fern_Clay_RenderType_ScissorEnd() { return CLAY_RENDER_COMMAND_TYPE_SCISSOR_END; }
int32_t Fern_Clay_RenderType_Custom() { return CLAY_RENDER_COMMAND_TYPE_CUSTOM; }

// Sizing type constants
int32_t Fern_Clay_SizingType_Fit() { return CLAY__SIZING_TYPE_FIT; }
int32_t Fern_Clay_SizingType_Grow() { return CLAY__SIZING_TYPE_GROW; }
int32_t Fern_Clay_SizingType_Percent() { return CLAY__SIZING_TYPE_PERCENT; }
int32_t Fern_Clay_SizingType_Fixed() { return CLAY__SIZING_TYPE_FIXED; }

} // extern "C"
