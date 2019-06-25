using System;
using System.Data;
using System.Configuration;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;

/// <summary>
/// Summary description for vieworder
/// </summary>
public partial class viewitem_cust : System.Web.UI.Page
{

    protected void Page_Load(object sender, EventArgs e)
    {
        
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Select";


        if (!Page.IsPostBack)
        {
            if (Session["User"] != null)
            {
                string vUserId = UserLogin.UserName;
                string vPage = "viewitem_cust_inv.aspx";
                string aUsers = null;
                string PrmComp = null;
                bool vCanCreate = false;
                bool vCanRun = false;
                bool vCanUpdate = false;
                bool vCanDelete = false;

                func1 f1 = new func1();
                f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);
                if (vCanRun == false)
                {
                    Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                    Response.Write("<script>window.location.href = 'login.aspx';</script>");

                }
                lblComp.Text = PrmComp;
                lblUser.Text = UserLogin.UserName;


            }
            if (FormView1.DataItemCount < 0)
            {
                addnewbutton.Visible = true;
                //Response.Write(FormView1.DataItemCount);
            }
            else
            {
                addnewbutton.Visible = false;

            }
        }

    }


    protected void LinkButton1_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
    }
    protected void hlnkLogOut_Click(object sender, EventArgs e)
    {
        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        if (Request.Cookies["showmenu"] != null)
        {
            Response.Cookies["showmenu"].Expires = DateTime.Now.AddDays(-1);
        }
        Response.Redirect(sLoginURL);
    }

    protected void lnk_list_click(object sender, EventArgs e)
    {
        Response.Redirect("item_cust_inv.aspx");
    }

    protected void lnk_view_click(object sender, EventArgs e)
    {
        Response.Redirect("viewitem_cust_inv.aspx");
    }
    protected void Insert_Button_Click(object sender, EventArgs e)
    {
        TextBox custno = (TextBox)FormView1.FindControl("vCustNumTextBox");
        TextBox revision = (TextBox)FormView1.FindControl("vRevisionTextBox");
        TextBox fgitem = (TextBox)FormView1.FindControl("vFgItmNumTextBox");
        TextBox custpart = (TextBox)FormView1.FindControl("vCustPartNumTextBox");
        TextBox apvendor = (TextBox)FormView1.FindControl("vCustVenCodeTextBox");
        TextBox palentid = (TextBox)FormView1.FindControl("vVendPlantIdTextBox");
        TextBox deptcode = (TextBox)FormView1.FindControl("vVendorDeptCodeTextBox");
        TextBox obsoletedt = (TextBox)FormView1.FindControl("vOsoleteDateTextBox");
        TextBox annual = (TextBox)FormView1.FindControl("vAnnUsageTextBox");
        TextBox hand = (TextBox)FormView1.FindControl("vCustHandQtyTextBox");


        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Add";
        ObjectDataSource1.SelectParameters["prmCustNum"].DefaultValue = custno.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRevision"].DefaultValue = revision.Text.Trim();
        ObjectDataSource1.SelectParameters["prmFgItmNum"].DefaultValue = fgitem.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCustPartNum"].DefaultValue = custpart.Text.Trim();
        ObjectDataSource1.SelectParameters["prmVendCode"].DefaultValue = apvendor.Text.Trim();
        ObjectDataSource1.SelectParameters["prmVendPlantId"].DefaultValue = palentid.Text.Trim();
        ObjectDataSource1.SelectParameters["prmVendDeptCode"].DefaultValue = deptcode.Text.Trim();
        ObjectDataSource1.SelectParameters["prmOsoleteDate"].DefaultValue = obsoletedt.Text.Trim();
        ObjectDataSource1.SelectParameters["prmAnnUsageQty"].DefaultValue = annual.Text.Trim();
        ObjectDataSource1.SelectParameters["prmOnHandQty"].DefaultValue = hand.Text.Trim();
        FormView1.ChangeMode(FormViewMode.ReadOnly);

    }
    protected void Update_Button_Click(object sender, EventArgs e)
    {
        Label reckey = (Label)FormView1.FindControl("updateLabel1");
        TextBox custno = (TextBox)FormView1.FindControl("vCustNumTextBox");
        TextBox revision = (TextBox)FormView1.FindControl("vRevisionTextBox");
        TextBox fgitem = (TextBox)FormView1.FindControl("vFgItmNumTextBox");
        TextBox custpart = (TextBox)FormView1.FindControl("vCustPartNumTextBox");
        TextBox apvendor = (TextBox)FormView1.FindControl("vCustVenCodeTextBox");
        TextBox palentid = (TextBox)FormView1.FindControl("vVendPlantIdTextBox");
        TextBox deptcode = (TextBox)FormView1.FindControl("vVendorDeptCodeTextBox");
        TextBox obsoletedt = (TextBox)FormView1.FindControl("vOsoleteDateTextBox");
        TextBox annual = (TextBox)FormView1.FindControl("vAnnUsageTextBox");
        TextBox hand = (TextBox)FormView1.FindControl("vCustHandQtyTextBox");
        

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Update";
        ObjectDataSource1.SelectParameters["prmRecId"].DefaultValue = reckey.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCustNum"].DefaultValue = custno.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRevision"].DefaultValue = revision.Text.Trim();
        ObjectDataSource1.SelectParameters["prmFgItmNum"].DefaultValue = fgitem.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCustPartNum"].DefaultValue = custpart.Text.Trim();
        ObjectDataSource1.SelectParameters["prmVendCode"].DefaultValue = apvendor.Text.Trim();
        ObjectDataSource1.SelectParameters["prmVendPlantId"].DefaultValue = palentid.Text.Trim();
        ObjectDataSource1.SelectParameters["prmVendDeptCode"].DefaultValue = deptcode.Text.Trim();
        ObjectDataSource1.SelectParameters["prmOsoleteDate"].DefaultValue = obsoletedt.Text.Trim();
        ObjectDataSource1.SelectParameters["prmAnnUsageQty"].DefaultValue = annual.Text.Trim();
        ObjectDataSource1.SelectParameters["prmOnHandQty"].DefaultValue = hand.Text.Trim();
        FormView1.ChangeMode(FormViewMode.ReadOnly);
        
    }
    protected void DeleteButton_Click(object sender, EventArgs e)
    {
        Label rec = (Label)FormView1.FindControl("Label1");
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];        
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Delete";
        ObjectDataSource1.SelectParameters["prmRecId"].DefaultValue = rec.Text.Trim();
        FormView1.ChangeMode(FormViewMode.ReadOnly);

    }
    protected void FormView1_PreRender(object sender, EventArgs e)
    {
        try
        {
            Label rec = (Label)FormView1.FindControl("Label1");
            Session["itemcustinv_no"] = rec.Text;

        }
        catch { }
    }
    protected void newbutton_click(object sender, EventArgs e)
    {
        FormView1.ChangeMode(FormViewMode.Insert);
        addnewbutton.Visible = false;
    }
    protected void Formview1_DataBound(object sender, EventArgs e)
    {
        if (FormView1.CurrentMode == FormViewMode.Edit)
        {
            TextBox arcode = (TextBox)FormView1.FindControl("vCustNumTextBox");
            arcode.Focus();
        }
        if (FormView1.CurrentMode == FormViewMode.Insert)
        {
            TextBox arcode = (TextBox)FormView1.FindControl("vCustNumTextBox");
            arcode.Focus();
        }
    }
    protected void CustNum_TextChange(object sender, EventArgs e)
    {
        TextBox custno = (TextBox)FormView1.FindControl("vCustNumTextBox");
        TextBox revision = (TextBox)FormView1.FindControl("vRevisionTextBox");
        TextBox fgitem = (TextBox)FormView1.FindControl("vFgItmNumTextBox");
        TextBox custpart = (TextBox)FormView1.FindControl("vCustPartNumTextBox");
        TextBox apvendor = (TextBox)FormView1.FindControl("vCustVenCodeTextBox");
        TextBox palentid = (TextBox)FormView1.FindControl("vVendPlantIdTextBox");
        TextBox deptcode = (TextBox)FormView1.FindControl("vVendorDeptCodeTextBox");

        UserClass UserLogin = (UserClass)Session["User"];
        try
        {
            LookUp lookup = new LookUp();
            DataSet dsestimate = new DataSet();
            dsestimate = lookup.Cross_Ref_Lookup("search", UserLogin.UserName, "custno", "EQUAL", custno.Text);

            custno.Text = dsestimate.Tables[0].Rows[0][0].ToString();
            apvendor.Text = dsestimate.Tables[0].Rows[0][1].ToString();
            revision.Focus();
        }
        catch { }


    }
    protected void FgItmNum_TextChange(object sender, EventArgs e)
    {
        TextBox custno = (TextBox)FormView1.FindControl("vCustNumTextBox");
        TextBox revision = (TextBox)FormView1.FindControl("vRevisionTextBox");
        TextBox fgitem = (TextBox)FormView1.FindControl("vFgItmNumTextBox");
        TextBox custpart = (TextBox)FormView1.FindControl("vCustPartNumTextBox");
        TextBox apvendor = (TextBox)FormView1.FindControl("vCustVenCodeTextBox");
        TextBox palentid = (TextBox)FormView1.FindControl("vVendPlantIdTextBox");
        TextBox deptcode = (TextBox)FormView1.FindControl("vVendorDeptCodeTextBox");

        UserClass UserLogin = (UserClass)Session["User"];
        try
        {
            LookUp lookup = new LookUp();
            DataSet dsestimate = new DataSet();
            dsestimate = lookup.SedsFGitemLookup("search", UserLogin.UserName, "item", "EQUAL", fgitem.Text);

            fgitem.Text = dsestimate.Tables[0].Rows[0][0].ToString();
            custpart.Text = dsestimate.Tables[0].Rows[0][1].ToString();
            custpart.Focus();
        }
        catch { }


    }
    protected void PlantId_TextChange(object sender, EventArgs e)
    {
        TextBox custno = (TextBox)FormView1.FindControl("vCustNumTextBox");
        TextBox revision = (TextBox)FormView1.FindControl("vRevisionTextBox");
        TextBox fgitem = (TextBox)FormView1.FindControl("vFgItmNumTextBox");
        TextBox custpart = (TextBox)FormView1.FindControl("vCustPartNumTextBox");
        TextBox apvendor = (TextBox)FormView1.FindControl("vCustVenCodeTextBox");
        TextBox palentid = (TextBox)FormView1.FindControl("vVendPlantIdTextBox");
        TextBox deptcode = (TextBox)FormView1.FindControl("vVendorDeptCodeTextBox");

        UserClass UserLogin = (UserClass)Session["User"];
        try
        {
            LookUp lookup = new LookUp();
            DataSet dsestimate = new DataSet();
            dsestimate = lookup.CustPlantLook("search", UserLogin.UserName,"", "Plantid", "EQUAL", palentid.Text, custno.Text.Trim());

            palentid.Text = dsestimate.Tables[0].Rows[0][1].ToString();
            deptcode.Text = dsestimate.Tables[0].Rows[0][2].ToString();
            deptcode.Focus();
        }
        catch { }


    }
}

