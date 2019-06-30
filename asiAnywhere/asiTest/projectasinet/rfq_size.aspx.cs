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
using Progress.Open4GL.Proxy;
using ASINET1;
using ASIDataNS;

/// <summary>
/// Summary description for Class1
/// </summary>
public partial class rfqsize: System.Web.UI.Page
{
    public rfqsize()
	{
		//
		// TODO: Add constructor logic here
		//
	}

    protected void Page_Load(object sender, EventArgs e)
    {
        Session["my_new_rfq"] = null;
        
        //ImageButton rfqsize = (ImageButton)Master.FindControl("rfq_size");
        //rfqsize.ImageUrl = "~/Images/rfqsize1.jpg";
        Label name = (Label)Master.FindControl("lbl_page");
        name.Text = "Rfq Size";
        FormView1.ChangeMode(FormViewMode.ReadOnly);
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource2.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;

        if (!Page.IsPostBack)
        {
            if (Session["User"] != null)
            {
                

                string vUserId = UserLogin.UserName;
                string vPage = "rfq_size.aspx";
                string aUsers = null;
                string PrmComp = null;
                bool vCanCreate = false;
                bool vCanRun = false;
                bool vCanUpdate = false;
                bool vCanDelete = false;


                func1 f1 = new func1();
                //Response.Write(Page);
                f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

                //lblComp.Text = PrmComp;
                //Response.Write(vCanRun);

                if (aUsers == "external")
                {
                    FormView1.Visible = false;
                    FormView3.Visible = true;
                }
                if (aUsers == "internal")
                {
                    FormView3.Visible = false;
                    FormView1.Visible = true;
                }
                if (vCanRun == true)
                {
                    //lnk_brwsorder.Visible = true;
                    //brwsorder.Visible = true;

                }
                if (vCanRun == false)
                {
                    Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                    Response.Write("<script>window.location.href = 'login.aspx';</script>");

                }
            }
        }
    }
    protected void UpdateButton_click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {


            string vUserId = UserLogin.UserName;
            string vPage = "releases.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;




            func1 f1 = new func1();
           
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);
           
        }
        Label Part = (Label)FormView1.FindControl("part_noTextBox");
        Label Name = (Label)FormView1.FindControl("i_nameTextBox");
       Label Style = (Label)FormView1.FindControl("styleTextBox");
        TextBox Tab = (TextBox)FormView1.FindControl("tab_inTextBox");
        TextBox Len = (TextBox)FormView1.FindControl("lenTextBox");
        TextBox Wid = (TextBox)FormView1.FindControl("widTextBox");
        TextBox Dep = (TextBox)FormView1.FindControl("depTextBox");
        TextBox Dust = (TextBox)FormView1.FindControl("dustTextBox");
        TextBox Panel = (TextBox)FormView1.FindControl("fpanelTextBox");
        TextBox Tuck = (TextBox)FormView1.FindControl("tuckTextBox");
        TextBox Adhesive = (TextBox)FormView1.FindControl("adhesiveTextBox");
        TextBox KWid = (TextBox)FormView1.FindControl("k_widTextBox");
        TextBox KLen = (TextBox)FormView1.FindControl("k_lenTextBox");
        TextBox Lock = (TextBox)FormView1.FindControl("lockTextBox");
        TextBox GlueLap = (TextBox)FormView1.FindControl("gluelapTextBox");
        TextBox Lin = (TextBox)FormView1.FindControl("lin_inTextBox");
        TextBox TWid = (TextBox)FormView1.FindControl("t_widTextBox");
        TextBox TLen = (TextBox)FormView1.FindControl("t_lenTextBox");
        Label dscr = (Label)FormView1.FindControl("vStyleDscrTextBox");
        TextBox Sqin = (TextBox)FormView1.FindControl("lv_sqinTextBox");
        

        //FormView1.ChangeMode(FormViewMode.Edit);

        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "UpdateSize";

       ObjectDataSource1.SelectParameters["PrmPartNo"].DefaultValue = Part.Text.Trim();
        ObjectDataSource1.SelectParameters["prmName"].DefaultValue = Name.Text.Trim();
        ObjectDataSource1.SelectParameters["prmStyle"].DefaultValue = Style.Text.Trim();
        ObjectDataSource1.SelectParameters["prmTab"].DefaultValue = Tab.Text.Trim();
        ObjectDataSource1.SelectParameters["prmLen"].DefaultValue = Len.Text.Trim();
        ObjectDataSource1.SelectParameters["prmWid"].DefaultValue = Wid.Text.Trim();
        ObjectDataSource1.SelectParameters["prmDep"].DefaultValue = Dep.Text.Trim();
        ObjectDataSource1.SelectParameters["prmDust"].DefaultValue = Dust.Text.Trim();
        ObjectDataSource1.SelectParameters["prmPanel"].DefaultValue = Panel.Text.Trim();
        ObjectDataSource1.SelectParameters["prmTuck"].DefaultValue = Tuck.Text.Trim();
        ObjectDataSource1.SelectParameters["prmAdhesive"].DefaultValue = Adhesive.Text.Trim();
        ObjectDataSource1.SelectParameters["prmKWid"].DefaultValue = KWid.Text.Trim();
        ObjectDataSource1.SelectParameters["prmKLen"].DefaultValue = KLen.Text.Trim();

        ObjectDataSource1.SelectParameters["prmLock"].DefaultValue = Lock.Text.Trim();
        ObjectDataSource1.SelectParameters["prmGluela"].DefaultValue = GlueLap.Text.Trim();
        ObjectDataSource1.SelectParameters["prmLin"].DefaultValue = Lin.Text.Trim();
        ObjectDataSource1.SelectParameters["prmTWid"].DefaultValue = TWid.Text.Trim();
        ObjectDataSource1.SelectParameters["prmTLen"].DefaultValue = TLen.Text.Trim();
        ObjectDataSource1.SelectParameters["prmdscr"].DefaultValue = dscr.Text.Trim();
        ObjectDataSource1.SelectParameters["prmSqin"].DefaultValue = Sqin.Text.Trim();
        
        
    }
    protected void FormView1_DataBound(object sender, EventArgs e)
    {
        if (FormView1.CurrentMode == FormViewMode.Edit)
        {
            TextBox len = (TextBox)FormView1.FindControl("lenTextBox");
            len.Focus();
        }
    }
}
