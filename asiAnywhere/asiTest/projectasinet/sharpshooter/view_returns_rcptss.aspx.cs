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

namespace projectasinet.sharpshooter
{
    public partial class view_returns_rcptss : System.Web.UI.Page
    {

        protected void Page_Load(object sender, EventArgs e)
        {

            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Select";
            if (Convert.ToString(Session["cons_recipt_list_add_new"]) == "Add")
            {
                Session["return_recipt_list_add_new"] = null;
                FormView1.ChangeMode(FormViewMode.Insert);
            }


            if (!Page.IsPostBack)
            {
                if (Session["User"] != null)
                {
                    string vUserId = UserLogin.UserName;
                    string vPage = "view_daily_receipt.aspx";
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

            }

        }


        protected void LinkButton1_Click(object sender, EventArgs e)
        {
            Response.Redirect("menu.aspx");
        }
        protected void hlnkLogOut_Click(object sender, EventArgs e)
        {
            string sLoginURL = ConfigurationManager.AppSettings["LoginFileSs"];
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
            Response.Redirect("returns_rcpt_listss.aspx");
        }

        protected void lnk_view_click(object sender, EventArgs e)
        {
            Response.Redirect("view_returns_rcptss.aspx");
        }
        protected void Insert_Button_Click(object sender, EventArgs e)
        {
            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];

            TextBox date = (TextBox)FormView1.FindControl("vDateTextBox");
            TextBox transtime = (TextBox)FormView1.FindControl("vTransTimeTextBox");
            TextBox tag = (TextBox)FormView1.FindControl("vTagTextBox");
            TextBox job = (TextBox)FormView1.FindControl("vJob_noTextBox");
            TextBox jobno2 = (TextBox)FormView1.FindControl("vJob_no2TextBox");
            TextBox item = (TextBox)FormView1.FindControl("vItemTextBox");
            TextBox itemname = (TextBox)FormView1.FindControl("vItemNameTextBox");
            TextBox loc = (TextBox)FormView1.FindControl("vLocTextBox");
            TextBox locbin = (TextBox)FormView1.FindControl("vLocBinTextBox");
            TextBox cases = (TextBox)FormView1.FindControl("vCasesTextBox");
            TextBox qtycas = (TextBox)FormView1.FindControl("vQtyCasTextBox");
            TextBox casunit = (TextBox)FormView1.FindControl("vCasUnitTextBox");
            TextBox partial = (TextBox)FormView1.FindControl("vPartialTextBox");
            TextBox stdcost = (TextBox)FormView1.FindControl("vStdCostTextBox");
            TextBox costuom = (TextBox)FormView1.FindControl("vCostUomTextBox");
            TextBox t_qty = (TextBox)FormView1.FindControl("vT_QtyTextBox");
            TextBox extcost = (TextBox)FormView1.FindControl("vExtCostTextBox");
            TextBox countqty = (TextBox)FormView1.FindControl("vinvnoTextBox");


            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Addnew";
            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource1.SelectParameters["prmFgItem"].DefaultValue = item.Text.Trim();
            ObjectDataSource1.SelectParameters["prmJobno"].DefaultValue = job.Text.Trim();
            ObjectDataSource1.SelectParameters["prmRcptDate"].DefaultValue = date.Text.Trim();
            ObjectDataSource1.SelectParameters["prmTagno"].DefaultValue = tag.Text.Trim();
            ObjectDataSource1.SelectParameters["prmTransTime"].DefaultValue = transtime.Text.Trim();
            ObjectDataSource1.SelectParameters["prmJob_no2"].DefaultValue = jobno2.Text.Trim();
            ObjectDataSource1.SelectParameters["prmName"].DefaultValue = itemname.Text.Trim();
            ObjectDataSource1.SelectParameters["prmLoc"].DefaultValue = loc.Text.Trim();
            ObjectDataSource1.SelectParameters["prmLocBin"].DefaultValue = locbin.Text.Trim();
            ObjectDataSource1.SelectParameters["prmCases"].DefaultValue = cases.Text.Trim();
            ObjectDataSource1.SelectParameters["prmQty_Cas"].DefaultValue = qtycas.Text.Trim();
            ObjectDataSource1.SelectParameters["prmCasUnit"].DefaultValue = casunit.Text.Trim();
            ObjectDataSource1.SelectParameters["prmPartial"].DefaultValue = partial.Text.Trim();
            ObjectDataSource1.SelectParameters["prmStdCost"].DefaultValue = stdcost.Text.Trim();
            ObjectDataSource1.SelectParameters["prmCost_Uom"].DefaultValue = costuom.Text.Trim();
            ObjectDataSource1.SelectParameters["prmTQty"].DefaultValue = t_qty.Text.Trim();
            ObjectDataSource1.SelectParameters["prmExtCost"].DefaultValue = extcost.Text.Trim();
            ObjectDataSource1.SelectParameters["prmInvNo"].DefaultValue = countqty.Text.Trim();

            FormView1.ChangeMode(FormViewMode.ReadOnly);



        }
        protected void Update_Button_Click(object sender, EventArgs e)
        {
            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];

            TextBox date = (TextBox)FormView1.FindControl("vDateTextBox");
            TextBox transtime = (TextBox)FormView1.FindControl("vTransTimeTextBox");
            TextBox tag = (TextBox)FormView1.FindControl("vTagTextBox");
            TextBox job = (TextBox)FormView1.FindControl("vJob_noTextBox");
            TextBox jobno2 = (TextBox)FormView1.FindControl("vJob_no2TextBox");
            TextBox item = (TextBox)FormView1.FindControl("vItemTextBox");
            TextBox itemname = (TextBox)FormView1.FindControl("vItemNameTextBox");
            TextBox loc = (TextBox)FormView1.FindControl("vLocTextBox");
            TextBox locbin = (TextBox)FormView1.FindControl("vLocBinTextBox");
            TextBox cases = (TextBox)FormView1.FindControl("vCasesTextBox");
            TextBox qtycas = (TextBox)FormView1.FindControl("vQtyCasTextBox");
            TextBox casunit = (TextBox)FormView1.FindControl("vCasUnitTextBox");
            TextBox partial = (TextBox)FormView1.FindControl("vPartialTextBox");
            TextBox stdcost = (TextBox)FormView1.FindControl("vStdCostTextBox");
            TextBox costuom = (TextBox)FormView1.FindControl("vCostUomTextBox");
            TextBox t_qty = (TextBox)FormView1.FindControl("vT_QtyTextBox");
            TextBox extcost = (TextBox)FormView1.FindControl("vExtCostTextBox");
            TextBox countqty = (TextBox)FormView1.FindControl("vinvnoTextBox");


            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Update";
            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource1.SelectParameters["prmFgItem"].DefaultValue = item.Text.Trim();
            ObjectDataSource1.SelectParameters["prmJobno"].DefaultValue = job.Text.Trim();
            ObjectDataSource1.SelectParameters["prmRcptDate"].DefaultValue = date.Text.Trim();
            ObjectDataSource1.SelectParameters["prmTagno"].DefaultValue = tag.Text.Trim();
            ObjectDataSource1.SelectParameters["prmTransTime"].DefaultValue = transtime.Text.Trim();
            ObjectDataSource1.SelectParameters["prmJob_no2"].DefaultValue = jobno2.Text.Trim();
            ObjectDataSource1.SelectParameters["prmName"].DefaultValue = itemname.Text.Trim();
            ObjectDataSource1.SelectParameters["prmLoc"].DefaultValue = loc.Text.Trim();
            ObjectDataSource1.SelectParameters["prmLocBin"].DefaultValue = locbin.Text.Trim();
            ObjectDataSource1.SelectParameters["prmCases"].DefaultValue = cases.Text.Trim();
            ObjectDataSource1.SelectParameters["prmQty_Cas"].DefaultValue = qtycas.Text.Trim();
            ObjectDataSource1.SelectParameters["prmCasUnit"].DefaultValue = casunit.Text.Trim();
            ObjectDataSource1.SelectParameters["prmPartial"].DefaultValue = partial.Text.Trim();
            ObjectDataSource1.SelectParameters["prmStdCost"].DefaultValue = stdcost.Text.Trim();
            ObjectDataSource1.SelectParameters["prmCost_Uom"].DefaultValue = costuom.Text.Trim();
            ObjectDataSource1.SelectParameters["prmTQty"].DefaultValue = t_qty.Text.Trim();
            ObjectDataSource1.SelectParameters["prmExtCost"].DefaultValue = extcost.Text.Trim();
            ObjectDataSource1.SelectParameters["prmInvNo"].DefaultValue = countqty.Text.Trim();

            FormView1.ChangeMode(FormViewMode.ReadOnly);

        }
        protected void DeleteButton_Click(object sender, EventArgs e)
        {
            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            Label seq = (Label)FormView1.FindControl("LabelvRno");
            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Deletercpt";
            ObjectDataSource1.SelectParameters["prmRecKey"].DefaultValue = seq.Text.Trim();

        }
        protected void FormView1_PreRender(object sender, EventArgs e)
        {
            try
            {
                Label sql = (Label)FormView1.FindControl("LabelvRno");
                Session["return_recept_list_seq"] = sql.Text;

            }
            catch { }
        }

        protected void FormView1_DataBound(object sender, EventArgs e)
        {
            if (FormView1.CurrentMode == FormViewMode.Edit)
            {
                TextBox tag = (TextBox)FormView1.FindControl("vTagTextBox");
                tag.Focus();

            }
            if (FormView1.CurrentMode == FormViewMode.Insert)
            {
                TextBox tag = (TextBox)FormView1.FindControl("vTagTextBox");
                tag.Focus();


                TextBox vdate = (TextBox)FormView1.FindControl("vDateTextBox");
                TextBox vtime = (TextBox)FormView1.FindControl("vTransTimeTextBox");
                vdate.Text = DateTime.Now.ToShortDateString();
                vtime.Text = DateTime.Now.ToString("HH:MM");


            }
        }



        protected void Cancel_button_click(object sender, EventArgs e)
        {
            FormView1.ChangeMode(FormViewMode.ReadOnly);
        }

        protected void totag_textboxChange(object sender, EventArgs e)
        {
            TextBox tag = (TextBox)FormView1.FindControl("vTagTextBox");

            TextBox toloc = (TextBox)FormView1.FindControl("vLoc2TextBox");
            TextBox tolocbbin = (TextBox)FormView1.FindControl("vLocBin2TextBox");
            TextBox totag = (TextBox)FormView1.FindControl("vTag2TextBox");

            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            if (totag.Text == "")
            {
                return;
            }
            try
            {
                browspo look = new browspo();
                DataSet ds = new DataSet();
                ds = look.SelecttrnstagLook("PoSearch", UserLogin.UserName, "Tag#", "EQUAL", tag.Text.Trim(), "", "cons");
                // Response.Write(ds.Tables[0].Rows[0][1].ToString());

                toloc.Text = ds.Tables[0].Rows[0][7].ToString();
                tolocbbin.Text = ds.Tables[0].Rows[0][8].ToString();

                totag.Focus();

            }
            catch
            {
                HttpContext.Current.Response.Write("<script>alert('Invalid To Tag Number')</script>");

            }

        }

        protected void Tagtextbox_Click(object sneder, EventArgs e)
        {

            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            TextBox date = (TextBox)FormView1.FindControl("vDateTextBox");
            TextBox transtime = (TextBox)FormView1.FindControl("vTransTimeTextBox");
            TextBox tag = (TextBox)FormView1.FindControl("vTagTextBox");
            TextBox job = (TextBox)FormView1.FindControl("vJob_noTextBox");
            TextBox jobno2 = (TextBox)FormView1.FindControl("vJob_no2TextBox");
            TextBox item = (TextBox)FormView1.FindControl("vItemTextBox");
            TextBox itemname = (TextBox)FormView1.FindControl("vItemNameTextBox");
            TextBox loc = (TextBox)FormView1.FindControl("vLocTextBox");
            TextBox locbin = (TextBox)FormView1.FindControl("vLocBinTextBox");
            TextBox cases = (TextBox)FormView1.FindControl("vCasesTextBox");
            TextBox qtycas = (TextBox)FormView1.FindControl("vQtyCasTextBox");
            TextBox casunit = (TextBox)FormView1.FindControl("vCasUnitTextBox");
            TextBox partial = (TextBox)FormView1.FindControl("vPartialTextBox");
            TextBox stdcost = (TextBox)FormView1.FindControl("vStdCostTextBox");
            TextBox costuom = (TextBox)FormView1.FindControl("vCostUomTextBox");
            TextBox t_qty = (TextBox)FormView1.FindControl("vT_QtyTextBox");
            TextBox extcost = (TextBox)FormView1.FindControl("vExtCostTextBox");
            TextBox countqty = (TextBox)FormView1.FindControl("vinvnoTextBox");
            if (tag.Text.Trim() == "")
            {
                tag.Focus();
                return;
            }
            try
            {
                browspo fgtaglook = new browspo();
                DataSet ds = new DataSet();

                ds = fgtaglook.SelectfgtakLook("PoSearch", UserLogin.UserName, "Tag#", "EQUAL", tag.Text.Trim(), "no", "");

                if (ds.Tables[0].Rows.Count == 0)
                {
                    HttpContext.Current.Response.Write("<script>alert('Invalid Tag#!' )</script>");
                    tag.Focus();
                }
                else
                {
                    loc.Text = ds.Tables[0].Rows[0][7].ToString();
                    locbin.Text = ds.Tables[0].Rows[0][8].ToString();
                    job.Text = ds.Tables[0].Rows[0][5].ToString();
                    jobno2.Text = ds.Tables[0].Rows[0][6].ToString();
                    item.Text = ds.Tables[0].Rows[0][3].ToString();
                    itemname.Text = ds.Tables[0].Rows[0][4].ToString();
                    cases.Text = ds.Tables[0].Rows[0][18].ToString();
                    qtycas.Text = ds.Tables[0].Rows[0][9].ToString();
                    casunit.Text = ds.Tables[0].Rows[0][12].ToString();
                    partial.Text = ds.Tables[0].Rows[0][24].ToString();
                    stdcost.Text = ds.Tables[0].Rows[0][15].ToString();
                    costuom.Text = ds.Tables[0].Rows[0][13].ToString();
                    t_qty.Text = ds.Tables[0].Rows[0][16].ToString();
                    tag.Focus();
                }
            }
            catch { }
        }


        protected void img_btn_add_click(object sender, EventArgs e)
        {
            FormView1.ChangeMode(FormViewMode.Insert);
        }
    }

}