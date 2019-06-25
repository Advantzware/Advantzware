using System;
using System.Data;
using System.Configuration;
using System.Collections;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;
using System.Diagnostics;
using System.IO;
using System.Text;

public partial class po_box_design : System.Web.UI.Page
{
    string COMPANY;
    protected void Page_Load(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        FormView1.ChangeMode(FormViewMode.ReadOnly);

        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Select";
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        //ImageButton boxdis = (ImageButton)Master.FindControl("view_box");
        //boxdis.ImageUrl = "Images/box_design_1.jpg";
        if (!Page.IsPostBack)
        {            
        }
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "Brwslist_po.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;


            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            Label compname = (Label)Master.FindControl("lblComp");
            Label username = (Label)Master.FindControl("lblUser");
            Label labelname = (Label)Master.FindControl("lbl_page");
            
            COMPANY = compname.Text;
            compname.Text = PrmComp;
            username.Text = UserLogin.UserName;
            
            if (aUsers == "external")
            {

            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }
        
    }
    protected void FormView1_DataBound(object sender, EventArgs e)
    {

        if (FormView1.CurrentMode == FormViewMode.ReadOnly)
        {
            TextBox score = (TextBox)FormView1.FindControl("vlscoreLabel");
            TextBox total = (TextBox)FormView1.FindControl("vlcum_scoreLabel");
            Label sc1label = (Label)FormView1.FindControl("sc1Label");
            Label sc2label = (Label)FormView1.FindControl("sc2Label");
            Label sc3label = (Label)FormView1.FindControl("sc3Label");
            Label sc4label = (Label)FormView1.FindControl("sc4Label");
            Label sc5label = (Label)FormView1.FindControl("sc5Label");
            Label tot1 = (Label)FormView1.FindControl("Label1");
            Label tot2 = (Label)FormView1.FindControl("Label2");
            Label tot3 = (Label)FormView1.FindControl("Label3");
            Label tot4 = (Label)FormView1.FindControl("Label4");
            Label tot5 = (Label)FormView1.FindControl("Label5");
            try
            {
                string score2 = score.Text;

                string strData = score2.Trim();
                string[] separator = new string[] { " " };
                string[] strSplitArr = strData.Split(separator, StringSplitOptions.RemoveEmptyEntries);
                strSplitArr = strData.Split(separator, StringSplitOptions.RemoveEmptyEntries);
                sc1label.Text = strSplitArr[0];
                sc2label.Text = strSplitArr[1];
                sc3label.Text = strSplitArr[2];
                sc4label.Text = strSplitArr[3];
                sc5label.Text = strSplitArr[4];
            }
            catch { }

            try
            {
                if (sc1label.Text == "")
                    sc1label.Visible = false;
                if (sc2label.Text == "")
                    sc2label.Visible = false;
                if (sc3label.Text == "")
                    sc3label.Visible = false;
                if (sc4label.Text == "")
                    sc4label.Visible = false;
                if (sc5label.Text == "")
                    sc5label.Visible = false;
            }
            catch { }

            try
            {
                string strData2 = total.Text.Trim();
                string[] separator2 = new string[] { " " };
                string[] strSplitArr2 = strData2.Split(separator2, StringSplitOptions.RemoveEmptyEntries);
                tot1.Text = strSplitArr2[0];
                tot2.Text = strSplitArr2[1];
                tot3.Text = strSplitArr2[2];
                tot4.Text = strSplitArr2[3];
                tot5.Text = strSplitArr2[4];
            }
            catch { }

            try
            {
                if (tot1.Text == "")
                    tot1.Visible = false;
                if (tot2.Text == "")
                    tot2.Visible = false;
                if (tot3.Text == "")
                    tot3.Visible = false;
                if (tot4.Text == "")
                    tot4.Visible = false;
                if (tot5.Text == "")
                    tot5.Visible = false;
            }
            catch { }

            TextBox wtotal = (TextBox)FormView1.FindControl("vwcum_scoreLabel");
            Label wtot1 = (Label)FormView1.FindControl("wtot1Label");
            Label wtot2 = (Label)FormView1.FindControl("wtot2Label");
            Label wtot3 = (Label)FormView1.FindControl("wtot3Label");
            try
            {
                string strwtotal = wtotal.Text.Trim();
                string[] septot = new string[] { " " };
                string[] strSplitot = strwtotal.Split(septot, StringSplitOptions.RemoveEmptyEntries);
                wtot1.Text = strSplitot[0];
                wtot2.Text = strSplitot[1];
                wtot3.Text = strSplitot[2];
            }
            catch { }

            try
            {
                if (wtot1.Text == "")
                    wtot1.Visible = false;
                if (wtot2.Text == "")
                    wtot2.Visible = false;
                if (wtot3.Text == "")
                    wtot3.Visible = false;
            }
            catch { }

            TextBox wscore = (TextBox)FormView1.FindControl("vwscoreLabel");
            Label wsc1 = (Label)FormView1.FindControl("wsc1Label");
            Label wsc2 = (Label)FormView1.FindControl("wsc2Label");
            Label wsc3 = (Label)FormView1.FindControl("wsc3Label");
            try
            {
                string strwscore = wscore.Text.Trim();
                string[] sepscor = new string[] { " " };
                string[] strSplitscore = strwscore.Split(sepscor, StringSplitOptions.RemoveEmptyEntries);
                wsc1.Text = strSplitscore[0];
                wsc2.Text = strSplitscore[1];
                wsc3.Text = strSplitscore[2];
            }
            catch { }

            try
            {
                if (wsc1.Text == "")
                    wsc1.Visible = false;
                if (wsc2.Text == "")
                    wsc2.Visible = false;
                if (wsc3.Text == "")
                    wsc3.Visible = false;
            }
            catch { }

            try
            {
                ImageButton view_box = (ImageButton)Master.FindControl("view_box");
                view_box.ImageUrl = "~/Images/box_design_1.jpg";
                Image movebutton = (Image)Master.FindControl("Image5");
                movebutton.Visible = false;
              

                Label img_3d = (Label)FormView1.FindControl("vbox_3d_imageLabel");
                Label box_img = (Label)FormView1.FindControl("vbox_imageLabel");
                box_img.Visible = true;
                img_3d.Visible = false;
                Image view_img = (Image)FormView1.FindControl("Image1");

                string imgpath = box_img.Text;
                string firstchar = imgpath.Substring(0, 1);
                string laststr = imgpath.Substring(1, imgpath.Length - 1);

                if (firstchar != "")
                {
                    imgpath = "D" + laststr;
                }

                if (File.Exists(imgpath))
                {
                    string imgfilename = Path.GetFileName(imgpath);
                    string dirPath = Server.MapPath(@"Images\Rcode\2D");
                    string destimgpath = Server.MapPath(@"Images\Rcode\2D" + "\\" + imgfilename);

                    string[] files = Directory.GetFiles(dirPath);
                    foreach (string file in files)
                        File.Delete(file);

                    File.Copy(imgpath, destimgpath);
                    view_img.ImageUrl = "~/Images/Rcode/2D/" + imgfilename;
                }
            }
            catch { Label6.Text = "No Record Found"; }

                    //view_img.ImageUrl = @"/images/Rsc3d.jpg";                                              
           // }
            


        }
    }
    
    
   
    

      
}

