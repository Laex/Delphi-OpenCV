// stdafx.h: ���������� ���� ��� ����������� ��������� ���������� ������
// ��� ���������� ������ ��� ����������� �������, ������� ����� ������������, ��
// �� ����� ����������
//

#pragma once

#include "targetver.h"

#define WIN32_LEAN_AND_MEAN             // ��������� ����� ������������ ���������� �� ���������� Windows

#define ICLASS_API extern "C" __declspec(dllexport) 
//__stdcall

// ����� ���������� Windows:
#include <windows.h>

#include "opencv2/core/core.hpp"
#include "opencv2/highgui/highgui.hpp"
#include "opencv2/objdetect.hpp"
#include "opencv2/contrib/contrib.hpp"


// TODO: ���������� ����� ������ �� �������������� ���������, ����������� ��� ���������